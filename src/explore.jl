
import Base: union, union!, ==

###
# TWO STATE OBJECTS
###

# See https://gist.github.com/fonsp/fc7ace2bd8a0dfc2f87d694336f6c04a for the performance of using storing `parts` as a `Tuple`.

"""
    FunctionName{N}

Represents a function name that may be qualified with module names.

# Fields
- `parts::NTuple{N,Symbol}`: the parts of the function name, e.g. `(:Base, :sqrt)` for `Base.sqrt`
- `joined::Symbol`: the joined representation with dots, e.g. `Symbol("Base.sqrt")`

# Examples
```julia
FunctionName(:sqrt)  # Simple function name
FunctionName(:Base, :sqrt)  # Qualified function name
FunctionName([:Base, :sqrt])  # From a vector
```
"""
struct FunctionName{N}
    parts::NTuple{N,Symbol}
    joined::Symbol
end

FunctionName(parts::Vector{Symbol}) = FunctionName(
    tuple(parts...),
    Symbol(join(parts, '.')),
)

FunctionName(parts::Symbol...) = FunctionName(
    parts,
    Symbol(join(parts, '.')),
)
FunctionName(part::Symbol) = FunctionName(
    (part,),
    part,
)
FunctionName() = FunctionName(
    (),
    Symbol(""),
)


"""
For an expression like `function Base.sqrt(x::Int)::Int x; end`, it has the following fields:
- `name::FunctionName`: the name, `[:Base, :sqrt]`
- `signature_hash::UInt`: a `UInt` that is unique for the type signature of the method declaration, ignoring argument names. In the example, this is equals `hash(ExpressionExplorer.canonalize( :(Base.sqrt(x::Int)::Int) ))`, see [`canonalize`](@ref) for more details.
"""
struct FunctionNameSignaturePair
    name::FunctionName
    signature_hash::UInt
end

Base.:(==)(a::FunctionNameSignaturePair, b::FunctionNameSignaturePair) = a.name == b.name && a.signature_hash == b.signature_hash
Base.hash(a::FunctionNameSignaturePair, h::UInt) = hash(:FunctionNameSignaturePair, hash(a.name, hash(a.signature_hash, h)))

"SymbolsState trickles _down_ the ASTree: it carries referenced and defined variables from endpoints down to the root."
Base.@kwdef mutable struct SymbolsState
    references::Set{Symbol} = Set{Symbol}()
    assignments::Set{Symbol} = Set{Symbol}()
    funccalls::Set{FunctionName} = Set{FunctionName}()
    funcdefs::Dict{FunctionNameSignaturePair,SymbolsState} = Dict{FunctionNameSignaturePair,SymbolsState}()
    macrocalls::Set{FunctionName} = Set{FunctionName}()
end

"ScopeState moves _up_ the ASTree: it carries scope information up towards the endpoints."
mutable struct ScopeState
    inglobalscope::Bool
    exposedglobals::Set{Symbol}
    hiddenglobals::Set{Symbol}
    definedfuncs::Set{Symbol}
end
ScopeState() = ScopeState(true, Set{Symbol}(), Set{Symbol}(), Set{Symbol}())

# The `union` and `union!` overloads define how two `SymbolsState`s or two `ScopeState`s are combined.

function union(a::Dict{FunctionNameSignaturePair,SymbolsState}, bs::Dict{FunctionNameSignaturePair,SymbolsState}...)
    union!(Dict{FunctionNameSignaturePair,SymbolsState}(), a, bs...)
end

function union!(a::Dict{FunctionNameSignaturePair,SymbolsState}, bs::Dict{FunctionNameSignaturePair,SymbolsState}...)
    for b in bs
        for (k, v) in b
            if haskey(a, k)
                a[k] = union!(a[k], v)
            else
                a[k] = v
            end
        end
        a
    end
    return a
end

function union(a::SymbolsState, b::SymbolsState)
    SymbolsState(a.references ∪ b.references, a.assignments ∪ b.assignments, a.funccalls ∪ b.funccalls, a.funcdefs ∪ b.funcdefs, a.macrocalls ∪ b.macrocalls)
end

function union!(a::SymbolsState, bs::SymbolsState...)
    mapreduce(b -> b.references, union!, bs; init=a.references)
    mapreduce(b -> b.assignments, union!, bs; init=a.assignments)
    mapreduce(b -> b.funccalls, union!, bs; init=a.funccalls)
    mapreduce(b -> b.funcdefs, union!, bs; init=a.funcdefs)
    mapreduce(b -> b.macrocalls, union!, bs; init=a.macrocalls)
    return a
end

function union!(a::Tuple{FunctionName,SymbolsState}, bs::Tuple{FunctionName,SymbolsState}...)
    a[1], mapreduce(last, union!, bs; init=a[2])
end

function union(a::ScopeState, b::ScopeState)
    SymbolsState(a.inglobalscope && b.inglobalscope, a.exposedglobals ∪ b.exposedglobals, a.hiddenglobals ∪ b.hiddenglobals)
end

function union!(a::ScopeState, bs::ScopeState...)
    a.inglobalscope &= mapreduce(b -> b.inglobalscope, &, bs; init=true)
    mapreduce(b -> b.exposedglobals, union!, bs; init=a.exposedglobals)
    mapreduce(b -> b.hiddenglobals, union!, bs; init=a.hiddenglobals)
    mapreduce(b -> b.definedfuncs, union!, bs; init=a.definedfuncs)
    return a
end

function ==(a::SymbolsState, b::SymbolsState)
    a.references == b.references && a.assignments == b.assignments && a.funccalls == b.funccalls && a.funcdefs == b.funcdefs && a.macrocalls == b.macrocalls
end

###
# HELPER FUNCTIONS
###

# from the source code: https://github.com/JuliaLang/julia/blob/master/src/julia-parser.scm#L9
const modifiers = [:(+=), :(-=), :(*=), :(/=), :(//=), :(^=), :(÷=), :(%=), :(<<=), :(>>=), :(>>>=), :(&=), :(⊻=), :(≔), :(⩴), :(≕)]
const modifiers_dotprefixed = [Symbol('.', m) for m in modifiers]

function will_assign_global(assignee::Symbol, scopestate::ScopeState)::Bool
    (scopestate.inglobalscope || assignee ∈ scopestate.exposedglobals) && (assignee ∉ scopestate.hiddenglobals || assignee ∈ scopestate.definedfuncs)
end

function will_assign_global(assignee::FunctionName, scopestate::ScopeState)::Bool
    if length(assignee.parts) == 0
        false
    elseif length(assignee.parts) > 1
        scopestate.inglobalscope
    else
        will_assign_global(assignee.parts[1], scopestate)
    end
end

function get_global_assignees(assignee_exprs, scopestate::ScopeState)::Set{Symbol}
    global_assignees = Set{Symbol}()
    for ae in assignee_exprs
        if isa(ae, Symbol)
            will_assign_global(ae, scopestate) && push!(global_assignees, ae)
        else
            if ae.head == :(::)
                will_assign_global(ae.args[1], scopestate) && push!(global_assignees, ae.args[1])
            else
                @warn "Unknown assignee expression" ae
            end
        end
    end
    return global_assignees
end

function get_assignees(ex::Expr)::Vector{Symbol}
    if ex.head == :tuple
        if length(ex.args) == 1 && Meta.isexpr(only(ex.args), :parameters)
            # e.g. (x, y) in the ex (; x, y) = (x = 5, y = 6, z = 7)
            args = only(ex.args).args
        else
            # e.g. (x, y) in the ex (x, y) = (1, 23)
            args = ex.args
        end
        mapfoldl(get_assignees, union!, args; init=Symbol[])
        # filter(s->s isa Symbol, ex.args)
    elseif ex.head == :(::)
        # TODO: type is referenced
        get_assignees(ex.args[1])
    elseif ex.head === :ref || ex.head === :(.)
        Symbol[]
    elseif ex.head === :...
        # Handles splat assignments. e.g. _, y... = 1:5
        args = ex.args
        mapfoldl(get_assignees, union!, args; init=Symbol[])
    elseif Meta.isexpr(ex, :escape, 1)
        get_assignees(ex.args[1])
    else
        @warn "unknown use of `=`. Assignee is unrecognised." ex
        Symbol[]
    end
end

# e.g. x = 123, but ignore _ = 456
get_assignees(ex::Symbol) = all_underscores(ex) ? Symbol[] : Symbol[ex]

# When you assign to a datatype like Int, String, or anything bad like that
# e.g. 1 = 2
# This is parsable code, so we have to treat it
get_assignees(::Any) = Symbol[]

all_underscores(s::Symbol) = all(isequal('_'), string(s))

# TODO: this should return a FunctionName, and use `split_funcname`.
"Turn :(A{T}) into :A."
function uncurly!(ex::Expr, scopestate::ScopeState)::Tuple{Symbol,SymbolsState}
    @assert ex.head === :curly
    symstate = SymbolsState()
    for curly_arg in ex.args[2:end]
        arg_name, arg_symstate = explore_funcdef!(curly_arg, scopestate)
        push!(scopestate.hiddenglobals, arg_name.joined)
        union!(symstate, arg_symstate)
    end
    Symbol(ex.args[1]), symstate
end

uncurly!(ex::Expr)::Tuple{Symbol,SymbolsState} = ex.args[1], SymbolsState()

uncurly!(s::Symbol, scopestate = nothing)::Tuple{Symbol,SymbolsState} = s, SymbolsState()



function join_funcnames(a::FunctionName, bs::FunctionName...)
	b = join_funcnames(bs...)
	FunctionName(
		(a.parts..., b.parts...),
		isempty(a.parts) ? b.joined : isempty(b.parts) ? a.joined : Symbol(a.joined, ".", b.joined)
	)
end
join_funcnames(x::FunctionName) = x


"Turn `:(Base.Submodule.f)` into `FunctionName(:Base, :Submodule, :f)` and `:f` into `FunctionName(:f)`."
function split_funcname(funcname_ex::Expr)::FunctionName
    if funcname_ex.head === :(.)
		mapfoldl(split_funcname, join_funcnames, funcname_ex.args; init=FunctionName())
    else
        # a call to a function that's not a global, like calling an array element: `funcs[12]()`
        # TODO: explore symstate!
        FunctionName()
    end
end

function split_funcname(funcname_ex::QuoteNode)::FunctionName
    split_funcname(funcname_ex.value)
end
function split_funcname(funcname_ex::GlobalRef)::FunctionName
    split_funcname(funcname_ex.name)
end

function split_funcname(funcname_ex::Symbol)::FunctionName
    FunctionName(funcname_ex |> without_dotprefix |> without_dotsuffix)
end

# this includes GlobalRef - it's fine that we don't recognise it, because you can't assign to a globalref?
function split_funcname(::Any)::FunctionName
    FunctionName()
end

# This allows users to create an Expr with a FunctionName as its name argument. This makes no sense to Julia but it's an easy way to tell ExpressionExplorer about the result that you want.
split_funcname(fn::FunctionName) = fn

function is_just_dots(ex::Expr)
    ex.head === :(.) && all(is_just_dots, ex.args)
end
is_just_dots(::Union{QuoteNode,Symbol,GlobalRef}) = true
is_just_dots(::Any) = false

"""Turn `Symbol(".+")` into `:(+)`"""
function without_dotprefix(funcname::Symbol)::Symbol
    fn_str = String(funcname)
    if length(fn_str) > 0 && fn_str[1] == '.'
        Symbol(fn_str[2:end])
    else
        funcname
    end
end

"""Turn `Symbol("sqrt.")` into `:sqrt`"""
function without_dotsuffix(funcname::Symbol)::Symbol
    fn_str = String(funcname)
    if length(fn_str) > 0 && fn_str[end] == '.'
        Symbol(fn_str[1:end-1])
    else
        funcname
    end
end

"""Generates a vector of all possible variants from a function name

```
julia> generate_funcnames([:Base, :Foo, :bar])
3-element Vector{Symbol}:
 Symbol("Base.Foo.bar")
 Symbol("Foo.bar")
 :bar
```

"""
function generate_funcnames(funccall::FunctionName)
    calls = Vector{FunctionName}(undef, length(funccall.parts) - 1)
    for i = length(funccall.parts):-1:2
        calls[i-1] = FunctionName(funccall.parts[i:end]...)
    end
    calls
end

# this is stupid -- désolé
function is_joined_funcname(joined::Symbol)
    joined !== :.. #= .. is a valid identifier 😐 =# && occursin('.', String(joined))
end


###
# MAIN RECURSIVE FUNCTION
###

# Spaghetti code for a spaghetti problem 🍝

# Possible leaf: value
# Like: a = 1
# 1 is a value (Int64)
function explore!(@nospecialize(value), scopestate::ScopeState)::SymbolsState
    # includes: LineNumberNode, Int64, String, Markdown.LaTeX, DataType and more.
    return SymbolsState()
end

# Possible leaf: symbol
# Like a = x
# x is a symbol
# We handle the assignment separately, and explore!(:a, ...) will not be called.
# Therefore, this method only handles _references_, which are added to the symbolstate, depending on the scopestate.
function explore!(sym::Symbol, scopestate::ScopeState)::SymbolsState
    if sym ∈ scopestate.hiddenglobals
        SymbolsState()
    else
        SymbolsState(references = Set([sym]))
    end
end

"""
Returns whether or not an assignment Expr(:(=),...) is assigning to a new function
  * f(x) = ...
  * f(x)::V = ...
  * f(::T) where {T} = ...
"""
is_function_assignment(ex::Expr)::Bool = ex.args[1] isa Expr && (ex.args[1].head === :call || ex.args[1].head === :where || (ex.args[1].head === :(::) && ex.args[1].args[1] isa Expr && ex.args[1].args[1].head === :call))

anonymous_name() = Symbol("__ExprExpl_anon__", rand(UInt64))

function explore_assignment!(ex::Expr, scopestate::ScopeState)::SymbolsState
    # Does not create scope

    if is_function_assignment(ex)
        # f(x, y) = x + y
        # Rewrite to:
        # function f(x, y) x + y end
        return explore!(Expr(:function, ex.args...), scopestate)
    end

    val = ex.args[2]
    # Handle generic types assignments A{B} = C{B, Int}
    if ex.args[1] isa Expr && ex.args[1].head::Symbol == :curly
        assignees_fn, symstate = explore_funcdef!(ex.args[1], scopestate)::Tuple{FunctionName, SymbolsState}
        assignees = Symbol[assignees_fn.parts[end]]
        innersymstate = union!(symstate, explore!(val, scopestate))
    else
        assignees = get_assignees(ex.args[1])
        symstate = innersymstate = explore!(val, scopestate)
    end

    global_assignees = get_global_assignees(assignees, scopestate)

    # If we are _not_ assigning a global variable, then this symbol hides any global definition with that name
    union!(scopestate.hiddenglobals, setdiff(assignees, global_assignees))
    assigneesymstate = explore!(ex.args[1], scopestate)

    union!(scopestate.hiddenglobals, global_assignees)
    union!(symstate.assignments, global_assignees)
    union!(symstate.references, setdiff(assigneesymstate.references, global_assignees))
    union!(symstate.funccalls, filter!(call -> length(call.parts) != 1 || call.joined ∉ global_assignees, assigneesymstate.funccalls))
    filter!(!all_underscores, symstate.references)  # Never record _ as a reference

    return symstate
end

function explore_modifiers!(ex::Expr, scopestate::ScopeState)
    # We change: a[1] += 123
    # to:        a[1] = a[1] + 123
    # We transform the modifier back to its operator
    # for when users redefine the + function

    operator = let
        s = string(ex.head)
        Symbol(s[1:prevind(s, lastindex(s))])
    end
    expanded_expr = Expr(:(=), ex.args[1], Expr(:call, operator, ex.args[1], ex.args[2]))
    return explore!(expanded_expr, scopestate)
end

function explore_dotprefixed_modifiers!(ex::Expr, scopestate::ScopeState)
    # We change: a[1] .+= 123
    # to:        a[1] .= a[1] + 123

    operator = Symbol(string(ex.head)[2:end-1])
    expanded_expr = Expr(:(.=), ex.args[1], Expr(:call, operator, ex.args[1], ex.args[2]))
    return explore!(expanded_expr, scopestate)
end

"Unspecialized mapfoldl."
function umapfoldl(@nospecialize(f::Function), itr::Vector; init=SymbolsState())
    if isempty(itr)
        return init
    else
        out = init
        for e in itr
            union!(out, f(e))
        end
        return out
    end
end

function explore_inner_scoped(ex::Expr, scopestate::ScopeState)::SymbolsState
    # Because we are entering a new scope, we create a copy of the current scope state, and run it through the expressions.
    innerscopestate = deepcopy(scopestate)
    innerscopestate.inglobalscope = false

    return umapfoldl(a -> explore!(a, innerscopestate), ex.args)
end

function explore_filter!(ex::Expr, scopestate::ScopeState)
    # In a filter, the assignment is the second expression, the condition the first
    args = collect(reverse(ex.args))
    umapfoldl(a -> explore!(a, scopestate), args)::SymbolsState
end

function explore_generator!(ex::Expr, scopestate::ScopeState)
    # Creates local scope

    # In a `generator`, a single expression is followed by the iterator assignments.
    # In a `for`, this expression comes at the end.

    # This is not strictly the normal form of a `for` but that's okay
    return explore!(Expr(:for, Iterators.reverse(ex.args[2:end])..., ex.args[1]), scopestate)
end

# explore! but only looking for macrocalls
# this is a heuristic to detect recursive macrocalls
# like `@eval @mymacro` which we couldn't
# naively detect with `macroexpand` preprocessor since
# we don't use macroexpand1
function explore_macrocalls!(ex::Expr, macrocalls)
    if Meta.isexpr(ex, :macrocall)
        push!(macrocalls, split_funcname(ex.args[1]))
    end
    for arg in ex.args
        explore_macrocalls!(arg, macrocalls)
    end
    macrocalls
end
explore_macrocalls!(_, macrocalls) = macrocalls


function explore_macrocall!(ex::Expr, scopestate::ScopeState)
    macrocalls = Set{FunctionName}()
    explore_macrocalls!(ex, macrocalls)
    return SymbolsState(;macrocalls)
end

function funcname_symstate!(funcname::FunctionName, scopestate::ScopeState)::SymbolsState
    if isempty(funcname.parts)
        SymbolsState()
    elseif length(funcname.parts) == 1
        if funcname.parts[1] ∈ scopestate.hiddenglobals
            SymbolsState()
        else
            SymbolsState(funccalls = Set{FunctionName}([funcname]))
        end
    elseif funcname.parts[1] ∈ scopestate.hiddenglobals
        SymbolsState()
    else
        SymbolsState(references = Set{Symbol}([funcname.parts[1]]), funccalls = Set{FunctionName}([funcname]))
    end
end

function explore_call!(ex::Expr, scopestate::ScopeState)::SymbolsState
    # Does not create scope

    if is_just_dots(ex.args[1])
        funcname = split_funcname(ex.args[1])::FunctionName
        symstate = funcname_symstate!(funcname, scopestate)

        # Explore code inside function arguments:
        union!(symstate, explore!(Expr(:block, ex.args[2:end]...), scopestate))

        # Make `@macroexpand` and `Base.macroexpand` reactive by referencing the first macro in the second
        # argument to the call.
        if ((:Base, :macroexpand) === funcname.parts || (:macroexpand,) === funcname.parts) &&
           length(ex.args) >= 3 &&
           ex.args[3] isa QuoteNode &&
           Meta.isexpr(ex.args[3].value, :macrocall)
            expanded_macro = split_funcname(ex.args[3].value.args[1])
            union!(symstate, SymbolsState(macrocalls = Set{FunctionName}([expanded_macro])))
        elseif (:BenchmarkTools, :generate_benchmark_definition) === funcname.parts &&
            length(ex.args) == 10
            block = Expr(:block,
                 map(ex.args[[8,7,9]]) do child
                    if (Meta.isexpr(child, :copyast, 1) && child.args[1] isa QuoteNode && child.args[1].value isa Expr)
                        child.args[1].value
                    else
                        nothing
                    end
                end...
            )
            union!(symstate, explore_inner_scoped(block, scopestate))
        end

        return symstate
    else
        return explore!(Expr(:block, ex.args...), scopestate)
    end
end

function explore_struct!(ex::Expr, scopestate::ScopeState)
    # Creates local scope

    structnameexpr = ex.args[2]
    structfields = ex.args[3].args

    equiv_func = Expr(:function, Expr(:call, structnameexpr, structfields...), Expr(:block, nothing))

    # struct should always be in Global state
    globalscopestate = deepcopy(scopestate)
    globalscopestate.inglobalscope = true

    # we register struct definitions as both a variable and a function. This is because deleting a struct is trickier than just deleting its methods.
    # Due to this, outer constructors have to be defined in the same cell where the struct is defined.
    # See https://github.com/fonsp/Pluto.jl/issues/732 for more details
    inner_symstate = explore!(equiv_func, globalscopestate)

    structname = first(keys(inner_symstate.funcdefs)).name.joined
    push!(inner_symstate.assignments, structname)
    return inner_symstate
end

function explore_abstract!(ex::Expr, scopestate::ScopeState)
    explore_struct!(Expr(:struct, false, ex.args[1], Expr(:block, nothing)), scopestate)
end

function explore_primitive!(ex::Expr, scopestate::ScopeState)
    type_name, type_body = ex.args
    symstate = explore_struct!(Expr(:struct, false, type_name, Expr(:block)), scopestate)
    union!(symstate, explore!(type_body, scopestate))
end

function explore_function_macro!(ex::Expr, scopestate::ScopeState)
    symstate = SymbolsState()
    # Creates local scope

    funcroot = ex.args[1]

    # Because we are entering a new scope, we create a copy of the current scope state, and run it through the expressions.
    innerscopestate = deepcopy(scopestate)
    innerscopestate.inglobalscope = false

    funcname, innersymstate = explore_funcdef!(funcroot, innerscopestate)::Tuple{FunctionName,SymbolsState}

    # Macro are called using @funcname, but defined with funcname. We need to change that in our scopestate
    # (The `!= 0` is for when the function named couldn't be parsed)
    if ex.head === :macro && length(funcname.parts) != 0
        funcname = FunctionName(Symbol('@', funcname.parts[1]))
        push!(innerscopestate.hiddenglobals, only(funcname.parts))
    elseif length(funcname.parts) == 1
        push!(scopestate.definedfuncs, funcname.parts[end])
        push!(scopestate.hiddenglobals, funcname.parts[end])
    elseif length(funcname.parts) > 1
        push!(symstate.references, funcname.parts[end-1]) # reference the module of the extended function
        push!(scopestate.hiddenglobals, funcname.parts[end-1])
    end

    union!(innersymstate, explore!(Expr(:block, ex.args[2:end]...), innerscopestate))
    funcnamesig = FunctionNameSignaturePair(funcname, hash(canonalize(funcroot)))

    if will_assign_global(funcname, scopestate)
        symstate.funcdefs[funcnamesig] = innersymstate
    else
        # The function is not defined globally. However, the function can still modify the global scope or reference globals, e.g.

        # let
        #     function f(x)
        #         global z = x + a
        #     end
        #     f(2)
        # end

        # so we insert the function's inner symbol state here, as if it was a `let` block.
        symstate = innersymstate
    end

    return symstate
end

function explore_try!(ex::Expr, scopestate::ScopeState)
    symstate = SymbolsState()

    # Handle catch first
    if ex.args[3] != false
        union!(symstate, explore_inner_scoped(ex.args[3], scopestate))
        # If we catch a symbol, it could shadow a global reference, remove it
        if ex.args[2] != false
            setdiff!(symstate.references, Symbol[ex.args[2]])
        end
    end

    # Handle the try block
    union!(symstate, explore_inner_scoped(ex.args[1], scopestate))

    # Handle finally
    if 4 <= length(ex.args) <= 5 && ex.args[4] isa Expr
        union!(symstate, explore_inner_scoped(ex.args[4], scopestate))
    end

    # Finally, handle else
    if length(ex.args) == 5
        union!(symstate, explore_inner_scoped(ex.args[5], scopestate))
    end

    return symstate
end

function explore_anonymous_function!(ex::Expr, scopestate::ScopeState)
    # Creates local scope

    tempname = anonymous_name()

    # We will rewrite this to a normal function definition, with a temporary name
    funcroot = ex.args[1]
    args_ex = if funcroot isa Symbol || (funcroot isa Expr && funcroot.head === :(::))
        [funcroot]
    elseif funcroot.head === :tuple || funcroot.head === :(...) || funcroot.head === :block
        funcroot.args
    else
        @error "Unknown lambda type"
    end

    equiv_func = Expr(:function, Expr(:call, tempname, args_ex...), ex.args[2])

    return explore!(equiv_func, scopestate)
end

function explore_global!(ex::Expr, scopestate::ScopeState)::SymbolsState
    # Does not create scope

    # global x, y, z
    if length(ex.args) > 1
        return umapfoldl(arg -> explore!(Expr(:global, arg), scopestate), ex.args)
    end

    # We have one of:
    # global x;
    # global x = 1;
    # global x += 1;

    # where x can also be a tuple:
    # global a,b = 1,2

    globalisee = ex.args[1]

    if isa(globalisee, Symbol)
        push!(scopestate.exposedglobals, globalisee)
        return SymbolsState()
    elseif isa(globalisee, Expr)
        # temporarily set inglobalscope to true
        old = scopestate.inglobalscope
        scopestate.inglobalscope = true
        result = explore!(globalisee, scopestate)
        scopestate.inglobalscope = old
        return result::SymbolsState
    else
        @error "unknown global use" ex
        return explore!(globalisee, scopestate)::SymbolsState
    end
end

function explore_local!(ex::Expr, scopestate::ScopeState)::SymbolsState
    # Does not create scope

    # Turn `local x, y` in `local x; local y
    if length(ex.args) > 1
        return umapfoldl(arg -> explore!(Expr(:local, arg), scopestate), ex.args)
    end

    localisee = ex.args[1]

    if isa(localisee, Symbol)
        push!(scopestate.hiddenglobals, localisee)
        return SymbolsState()
    elseif isa(localisee, Expr) && (localisee.head === :(=) || localisee.head in modifiers)
        union!(scopestate.hiddenglobals, get_assignees(localisee.args[1]))
        return explore!(localisee, scopestate)::SymbolsState
    else
        @warn "unknown local use" ex
        return explore!(localisee, scopestate)::SymbolsState
    end
end

function explore_tuple!(ex::Expr, scopestate::ScopeState)::SymbolsState
    # Does not create scope

    # There are two (legal) cases:
    # 1. Creating a tuple:
    #   (a, b, c, 1, f()...)
    # 2. Creating a named tuple (contains at least one Expr(:(=))):
    #   (a=1, b=2, c=3, d, f()...)

    # !!! Note that :(a, b = 1, 2) is the definition of a named tuple
    # with fields :a, :b and :2 and not a multiple assignments to a and b which
    # would always be a :(=) with tuples for the lhs and/or rhs.
    # Using Meta.parse() (like Pluto does) or using a quote block
    # returns the assignment version.
    #
    # julia> eval(:(a, b = 1, 2)) # Named tuple
    # ERROR: syntax: invalid named tuple element "2"
    #
    # julia> eval(Meta.parse("a, b = 1, 2")) # Assignment to a and b
    # (1, 2)
    #
    # julia> Meta.parse("a, b = 1, 2").head, :(a, b = 1, 2).head
    # (:(=), :tuple)

    return umapfoldl(a -> explore!(to_kw(a), scopestate), ex.args)
end

function explore_broadcast!(ex::Expr, scopestate::ScopeState)
    # pointwise function call, e.g. sqrt.(nums)
    # we rewrite to a regular call

    return explore!(Expr(:call, ex.args[1], ex.args[2].args...), scopestate)
end

function explore_load!(ex::Expr, scopestate::ScopeState)
    imports = if ex.args[1].head === :(:)
        ex.args[1].args[2:end]
    else
        ex.args
    end

    packagenames = map(e -> e.args[end], imports)

    return SymbolsState(assignments = Set{Symbol}(packagenames))::SymbolsState
end

function explore_quote!(ex::Expr, scopestate::ScopeState)
    # Look through the quote and only returns explore! deeper into :$'s
    # I thought we need to handle strings in the same way,
    #   but strings do just fine with the catch all at the end
    #   and actually strings don't always have a :$ expression, sometimes just
    #   plain Symbols (which we should then be interpreted as variables,
    #     which is different to how we handle Symbols in quote'd expressions)
    return explore_interpolations!(ex.args[1], scopestate)::SymbolsState
end

function explore_module!(ex::Expr, scopestate::ScopeState)
    # Does create it's own scope, but can import from outer scope, that's what `explore_module_definition!` is for
    symstate = explore_module_definition!(ex, scopestate)
    return union(symstate, SymbolsState(assignments = Set{Symbol}([ex.args[2]])))::SymbolsState
end

function explore_fallback!(ex::Expr, scopestate::ScopeState)
    # fallback, includes:
    # begin, block, do, toplevel, const
    # (and hopefully much more!)

    # Does not create scope (probably)

    return umapfoldl(a -> explore!(a, scopestate), ex.args)
end

# General recursive method. Is never a leaf.
# Modifies the `scopestate`.
function explore!(ex::Expr, scopestate::ScopeState)::SymbolsState
    if ex.head === :(=)
        return explore_assignment!(ex, scopestate)
    elseif ex.head in modifiers
        return explore_modifiers!(ex, scopestate)
    elseif ex.head in modifiers_dotprefixed
        return explore_dotprefixed_modifiers!(ex, scopestate)
    elseif ex.head === :let || ex.head === :for || ex.head === :while
        # Creates local scope
        return explore_inner_scoped(ex, scopestate)
    elseif ex.head === :filter
        return explore_filter!(ex, scopestate)
    elseif ex.head === :generator
        return explore_generator!(ex, scopestate)
    elseif ex.head === :macrocall
        return explore_macrocall!(ex, scopestate)
    elseif ex.head === :call
        return explore_call!(ex, scopestate)
    elseif Meta.isexpr(ex, :parameters)
        return umapfoldl(a -> explore!(to_kw(a), scopestate), ex.args)
    elseif ex.head === :kw
        return explore!(ex.args[2], scopestate)
    elseif ex.head === :struct
        return explore_struct!(ex, scopestate)
    elseif ex.head === :primitive
        return explore_primitive!(ex, scopestate)
    elseif ex.head === :abstract
        return explore_abstract!(ex, scopestate)
    elseif ex.head === :function || ex.head === :macro
        return explore_function_macro!(ex, scopestate)
    elseif ex.head === :try
        return explore_try!(ex, scopestate)
    elseif ex.head === :(->)
        return explore_anonymous_function!(ex, scopestate)
    elseif ex.head === :global
        return explore_global!(ex, scopestate)
    elseif ex.head === :local
        return explore_local!(ex, scopestate)
    elseif ex.head === :tuple
        return explore_tuple!(ex, scopestate)
    elseif Meta.isexpr(ex, :(.), 2) && ex.args[2] isa Expr && ex.args[2].head === :tuple
        return explore_broadcast!(ex, scopestate)
    elseif ex.head === :using || ex.head === :import
        return explore_load!(ex, scopestate)
    elseif ex.head === :quote
        return explore_quote!(ex, scopestate)
    elseif ex.head === :module
        return explore_module!(ex, scopestate)
    elseif Meta.isexpr(ex, Symbol("'"), 1)
        # a' corresponds to adjoint(a)
        return explore!(Expr(:call, :adjoint, ex.args[1]), scopestate)
    elseif Meta.isexpr(ex, :meta) || Meta.isexpr(ex, :inbounds)
        return SymbolsState()
    else
        return explore_fallback!(ex, scopestate)
    end
end

"""
Goes through a module definition, and picks out `import ..x`'s, which are references to the outer module.
We need `module_depth + 1` dots before the specifier, so nested modules can still access Pluto.
"""
function explore_module_definition!(ex::Expr, scopestate; module_depth::Number = 0)
    if ex.head === :using || ex.head === :import
        # We don't care about anything after the `:` here
        import_names = if ex.args[1].head === :(:)
            [ex.args[1].args[1]]
        else
            ex.args
        end


        symstate = SymbolsState()
        for import_name_expr in import_names
            if (
                Meta.isexpr(import_name_expr, :., module_depth + 2) &&
                all(x -> x == :., import_name_expr.args[begin:end-1]) &&
                import_name_expr.args[end] isa Symbol
            )
                # Theoretically it could still use an assignment from the same cell, if it weren't
                # for the fact that modules need to be top level, and we don't support multiple (toplevel) expressions in a cell yet :D
                push!(symstate.references, import_name_expr.args[end])
            end

        end

        return symstate
    elseif ex.head === :module
        # Explorer the block inside with one more depth added
        return explore_module_definition!(ex.args[3], scopestate, module_depth = module_depth + 1)
    elseif ex.head === :quote
        # TODO? Explore interpolations, modules can't be in interpolations, but `import`'s can >_>
        return SymbolsState()
    else
        # Go deeper
        return umapfoldl(a -> explore_module_definition!(a, scopestate, module_depth = module_depth), ex.args)
    end
end
explore_module_definition!(expr, scopestate; module_depth::Number = 1) = SymbolsState()


"Go through a quoted expression and use explore! for :\$ expressions"
function explore_interpolations!(ex::Expr, scopestate)
    if ex.head == :$
        return explore!(ex.args[1], scopestate)::SymbolsState
    else
        # We are still in a quote, so we do go deeper, but we keep ignoring everything except :$'s
        return umapfoldl(a -> explore_interpolations!(a, scopestate), ex.args)
    end
end
explore_interpolations!(anything_else, scopestate) = SymbolsState()

function to_kw(ex::Expr)
    if Meta.isexpr(ex, :(=))
        Expr(:kw, ex.args...)
    else
        ex
    end
end
to_kw(x) = x

unescape(x::Expr) = Meta.isexpr(x, :escape) ? x.args[1] : x
unescape(x) = x

"""
Return the function name and the SymbolsState from argument defaults. Add arguments as hidden globals to the `scopestate`.

Is also used for `struct` and `abstract`.
"""
function explore_funcdef!(ex::Expr, scopestate::ScopeState)::Tuple{FunctionName,SymbolsState}
    if ex.head === :call
        params_to_explore = ex.args[2:end]
        # Using the keyword args syntax f(;y) the :parameters node is the first arg in the AST when it should
        # be explored last. We change from (parameters, ...) to (..., parameters)
        if length(params_to_explore) >= 2 && params_to_explore[1] isa Expr && params_to_explore[1].head === :parameters
            params_to_explore = [params_to_explore[2:end]..., params_to_explore[1]]
        end

        # Handle struct as callables, `(obj::MyType)(a, b) = ...`
        # or `function (obj::MyType)(a, b) ...; end` by rewriting it as:
        # function MyType(obj, a, b) ...; end
        funcroot = ex.args[1]
        if Meta.isexpr(funcroot, :(::))
            if last(funcroot.args) isa Symbol
                return explore_funcdef!(Expr(:call, reverse(funcroot.args)..., params_to_explore...), scopestate)
            else
                # Function call as type: (obj::typeof(myotherobject))()
                symstate = explore!(last(funcroot.args), scopestate)
                name, declaration_symstate = if length(funcroot.args) == 1
                    explore_funcdef!(Expr(:call, anonymous_name(), params_to_explore...), scopestate)
                else
                    explore_funcdef!(Expr(:call, anonymous_name(), first(funcroot.args), params_to_explore...), scopestate)
                end
                return name, union!(symstate, declaration_symstate)
            end
        end

        # get the function name
        name, symstate = explore_funcdef!(funcroot, scopestate)
        # and explore the function arguments
        return umapfoldl(a -> explore_funcdef!(a, scopestate), params_to_explore; init=(name, symstate))
    elseif ex.head === :(::) || ex.head === :kw || ex.head === :(=)
        # Treat custom struct constructors as a local scope function
        if ex.head === :(=) && is_function_assignment(ex)
            symstate = explore!(ex, scopestate)
            return FunctionName(), symstate
        end

        # account for unnamed params, like in f(::Example) = 1
        if ex.head === :(::) && length(ex.args) == 1
            symstate = explore!(ex.args[1], scopestate)

            return FunctionName(), symstate
        end

        # For a() = ... in a struct definition
        if Meta.isexpr(ex, :(=), 2) && Meta.isexpr(ex.args[1], :call)
            name, symstate = explore_funcdef!(ex.args[1], scopestate)
            union!(symstate, explore!(ex.args[2], scopestate))
            return name, symstate
        end

        # recurse by starting by the right hand side because f(x=x) references the global variable x
        rhs_symstate = if length(ex.args) > 1
            # use `explore!` (not `explore_funcdef!`) to explore the argument's default value - these can contain arbitrary expressions
            explore!(ex.args[2], scopestate)
        else
            SymbolsState()
        end
        name, symstate = explore_funcdef!(ex.args[1], scopestate)
        union!(symstate, rhs_symstate)

        return name, symstate

    elseif ex.head === :where
        # function(...) where {T, S <: R, U <: A.B}
        # supertypes `R` and `A.B` are referenced
        supertypes_symstate = SymbolsState()
        for a in ex.args[2:end]
            name, inner_symstate = explore_funcdef!(a, scopestate)
            if length(name.parts) == 1
                push!(scopestate.hiddenglobals, name.parts[1])
            end
            union!(supertypes_symstate, inner_symstate)
        end
        # recurse
        name, symstate = explore_funcdef!(ex.args[1], scopestate)
        union!(symstate, supertypes_symstate)
        return name, symstate

    elseif ex.head === :(<:)
        # for use in `struct`, `abstract` and `primitive`
        name, symstate = uncurly!(unescape(ex.args[1]), scopestate)
        if length(ex.args) != 1
            union!(symstate, explore!(ex.args[2], scopestate))
        end
        return FunctionName(name), symstate

    elseif ex.head === :curly
        name, symstate = uncurly!(ex, scopestate)
        return FunctionName(name), symstate

    elseif Meta.isexpr(ex, :parameters)
        init = (FunctionName(), SymbolsState())
        return umapfoldl(a -> explore_funcdef!(to_kw(a), scopestate), ex.args; init)

    elseif ex.head === :tuple
        init = (FunctionName(), SymbolsState())
        return umapfoldl(a -> explore_funcdef!(a, scopestate), ex.args; init)

    elseif ex.head === :(.)
        return split_funcname(ex), SymbolsState()

    elseif ex.head === :(...)
        return explore_funcdef!(ex.args[1], scopestate)
    elseif ex.head === :escape
        return explore_funcdef!(ex.args[1], scopestate)
    else
        return FunctionName(), explore!(ex, scopestate)
    end
end

function explore_funcdef!(ex::QuoteNode, scopestate::ScopeState)::Tuple{FunctionName,SymbolsState}
    explore_funcdef!(ex.value, scopestate)
end

function explore_funcdef!(ex::Symbol, scopestate::ScopeState)::Tuple{FunctionName,SymbolsState}
    push!(scopestate.hiddenglobals, ex)
    FunctionName(ex |> without_dotprefix |> without_dotsuffix), SymbolsState()
end

function explore_funcdef!(::Any, ::ScopeState)::Tuple{FunctionName,SymbolsState}
    FunctionName(), SymbolsState()
end

###
# CANONICALIZE FUNCTION DEFINITIONS
###

"""
Turn a function definition expression (`Expr`) into a "canonical" form, in the sense that two methods that would evaluate to the same method signature have the same canonical form. Part of a solution to https://github.com/fonsp/Pluto.jl/issues/177. Such a canonical form cannot be achieved statically with 100% correctness (impossible), but we can make it good enough to be practical.


# Wait, "evaluate to the same method signature"?

In Pluto, you cannot do definitions of **the same global variable** in different cells. This is needed for reactivity to work, and it avoid ambiguous notebooks and stateful stuff. This rule used to also apply to functions: you had to place all methods of a function in one cell. (Go and read more about methods in Julia if you haven't already.) But this is quite annoying, especially because multiple dispatch is so important in Julia code. So we allow methods of the same function to be defined across multiple cells, but we still want to throw errors when you define **multiple methods with the same signature**, because one overrides the other. For example:
```julia
julia> f(x) = 1
f (generic function with 1 method)

julia> f(x) = 2
f (generic function with 1 method)
``

After adding the second method, the function still has only 1 method. This is because the second definition overrides the first one, instead of being added to the method table. This example should be illegal in Julia, for the same reason that `f = 1` and `f = 2` is illegal. So our problem is: how do we know that two cells will define overlapping methods? 

Ideally, we would just evaluate the user's code and **count methods** afterwards, letting Julia do the work. Unfortunately, we need to know this info _before_ we run cells, otherwise we don't know in which order to run a notebook! There are ways to break this circle, but it would complicate our process quite a bit.

Instead, we will do _static analysis_ on the function definition expressions to determine whether they overlap. This is non-trivial. For example, `f(x)` and `f(y::Any)` define the same method. Trickier examples are here: https://github.com/fonsp/Pluto.jl/issues/177#issuecomment-645039993

# Wait, "function definition expressions"?
For example:

```julia
e = :(function f(x::Int, y::String)
        x + y
    end)

dump(e, maxdepth=2)

#=
gives:

Expr
  head: Symbol function
  args: Array{Any}((2,))
    1: Expr
    2: Expr
=#
```

This first arg is the function head:

```julia
e.args[1] == :(f(x::Int, y::String))
```

# Mathematics
Our problem is to find a way to compute the equivalence relation ~ on `H × H`, with `H` the set of function head expressions, defined as:

`a ~ b` iff evaluating both expressions results in a function with exactly one method.

_(More precisely, evaluating `Expr(:function, x, Expr(:block))` with `x ∈ {a, b}`.)_

The equivalence sets are isomorphic to the set of possible Julia methods.

Instead of finding a closed form algorithm for `~`, we search for a _canonical form_: a function `canonical: H -> H` that chooses one canonical expression per equivalence class. It has the property 
    
`canonical(a) = canonical(b)` implies `a ~ b`.

We use this **canonical form** of the function's definition expression as its "signature". We compare these canonical forms when determining whether two function expressions will result in overlapping methods.

# Example
```julia
e1 = :(f(x, z::Any))
e2 = :(g(x, y))

canonalize(e1) == canonalize(e2)
```

```julia
e1 = :(f(x))
e2 = :(g(x, y))

canonalize(e1) != canonalize(e2)
```

```julia
e1 = :(f(a::X, b::wow(ie), c,      d...; e=f) where T)
e2 = :(g(z::X, z::wow(ie), z::Any, z...     ) where T)

canonalize(e1) == canonalize(e2)
```
"""
function canonalize(ex::Expr)
    if ex.head == :where
        Expr(:where, canonalize(ex.args[1]), ex.args[2:end]...)
    elseif ex.head == :call || ex.head == :tuple
        skip_index = ex.head == :call ? 2 : 1
        # ex.args[1], if ex.head == :call this is the function name, we dont want it

        interesting = filter(ex.args[skip_index:end]) do arg
            !(arg isa Expr && arg.head == :parameters)
        end

        hide_argument_name.(interesting)
    elseif ex.head == :(::)
        canonalize(ex.args[1])
    elseif ex.head == :curly || ex.head == :(<:)
        # for struct definitions, which we hackily treat as functions
        nothing
    else
        @error "Failed to canonalize this strange looking function" ex
        nothing
    end
end

# for `function g end`
canonalize(::Symbol) = nothing

function hide_argument_name(ex::Expr)
    if ex.head == :(::) && length(ex.args) > 1
        Expr(:(::), nothing, ex.args[2:end]...)
    elseif ex.head == :(...)
        Expr(:(...), hide_argument_name(ex.args[1]))
    elseif ex.head == :kw
        Expr(:kw, hide_argument_name(ex.args[1]), nothing)
    else
        ex
    end
end
hide_argument_name(::Symbol) = Expr(:(::), nothing, :Any)
hide_argument_name(x::Any) = x

###
# UTILITY FUNCTIONS
###

function handle_recursive_functions!(symstate::SymbolsState)
    # We do something special to account for recursive functions:
    # If a function `f` calls a function `g`, and both are defined inside this cell, the reference to `g` inside the symstate of `f` will be deleted.
    # The motivitation is that normally, an assignment (or function definition) will add that symbol to a list of 'hidden globals' - any future references to that symbol will be ignored. i.e. the _local definition hides a global_.
    # In the case of functions, you can reference functions and variables that do not yet exist, and so they won't be in the list of hidden symbols when the function definition is analysed. 
    # Of course, our method will fail if a referenced function is defined both inside the cell **and** in another cell. However, this will lead to a MultipleDefinitionError before anything bad happens.
    K = keys(symstate.funcdefs)
    for (func, inner_symstate) in symstate.funcdefs
        inner_symstate.references = setdiff(inner_symstate.references, K)
        inner_symstate.funccalls = setdiff(inner_symstate.funccalls, K)
    end
    return nothing
end

"""
compute_symbols_state(ex::Any)::SymbolsState

Return the global references, assignment, function calls and function definitions inside an arbitrary expression, in a `SymbolsState` object.
"""
function compute_symbols_state(ex::Any)::SymbolsState
    try
        compute_symbolreferences(ex)
    catch e
        if e isa InterruptException
            rethrow(e)
        end
        @error "Expression explorer failed on: " ex exception=(e,catch_backtrace())

        SymbolsState(references = Set{Symbol}([:fake_reference_to_prevent_it_from_looking_like_a_text_only_cell]))
    end
end

function compute_symbolreferences(ex::Any)::SymbolsState
    symstate = explore!(ex, ScopeState())
    handle_recursive_functions!(symstate)
    return symstate
end

@deprecate try_compute_symbolreferences(args...) compute_symbols_state(args...)




