# ExpressionExplorer.jl
 
Find all variables _referenced_ and _defined_ in an expression. This package is used internally by Pluto to find links between cells.

### Quick example

```julia
julia> using ExpressionExplorer

julia> ex = :(const words = split(line));

julia> node = ExpressionExplorer.compute_reactive_node(ex);

julia> node.references
Set{Symbol} with 2 elements:
  :line
  :split

julia> node.definitions
Set{Symbol} with 1 element:
  :words
```

# API

The main function to use is `compute_reactive_node(expression)`, which returns a `ReactiveNode`. There is also a more low-level API available: `compute_symbols_state` returning a `SymbolsSate`.

## High-level: `ReactiveNode`

If you are interested in the *dependencies* between expressions, then you should compute the `ReactiveNode` for each expression. This is a data structure that looks like:

```julia
Base.@kwdef struct ReactiveNode
    # core fields:
    references::Set{Symbol} = Set{Symbol}()
    definitions::Set{Symbol} = Set{Symbol}()
    
    # more advanced fields:
    soft_definitions::Set{Symbol} = Set{Symbol}()
    funcdefs_with_signatures::Set{FunctionNameSignaturePair} = Set{FunctionNameSignaturePair}()
    funcdefs_without_signatures::Set{Symbol} = Set{Symbol}()
    macrocalls::Set{Symbol} = Set{Symbol}()
end
```

You can use the function `compute_reactive_node(expression)` to explore an expression and generate the resulting `ReactiveNode`.

### Example for `compute_reactive_node`


```julia
julia> # the two expressions that we will use in this example:

julia> e1 = :(weather = magic() + science);

julia> e2 = :(weather() = magic() + science);
```

```julia
julia> r1 = ExpressionExplorer.compute_reactive_node(e1)
ExpressionExplorer.ReactiveNode(Set([:+, :magic, :science]), Set([:weather]), Set{Symbol}(), Set{ExpressionExplorer.FunctionNameSignaturePair}(), Set{Symbol}(), Set{Symbol}())

julia> r1.definitions
Set{Symbol} with 1 element:
  :weather

julia> r1.references
Set{Symbol} with 3 elements:
  :+
  :magic
  :science

julia> r1.funcdefs_without_signatures
Set{Symbol}()
```

```julia
julia> r2 = ExpressionExplorer.compute_reactive_node(e2)
ExpressionExplorer.ReactiveNode(Set([:+, :magic, :science]), Set{Symbol}(), Set{Symbol}(), Set(ExpressionExplorer.FunctionNameSignaturePair[ExpressionExplorer.FunctionNameSignaturePair([:weather], 0xa2e6e5b3d2eee6b5)]), Set([:weather]), Set{Symbol}())

julia> r2.definitions
Set{Symbol}()

julia> r2.references
Set{Symbol} with 3 elements:
  :+
  :magic
  :science

julia> r2.funcdefs_without_signatures
Set{Symbol} with 1 element:
  :weather
```



## Low-level: `SymbolsSate`

If you are not interested in just the *dependencies* between expressions, there is a more low-level data structure available. (We include it for completeness, but Pluto does not use this data, except to generate a `ReactiveNode`.)

The function `compute_symbols_state` take an expression as argument, and returns a `SymbolsState`.

```julia
Base.@kwdef mutable struct SymbolsState
    references::Set{Symbol} = Set{Symbol}()
    assignments::Set{Symbol} = Set{Symbol}()
    funccalls::Set{FunctionName} = Set{FunctionName}()
    funcdefs::Dict{FunctionNameSignaturePair,SymbolsState} = Dict{FunctionNameSignaturePair,SymbolsState}()
    macrocalls::Set{FunctionName} = Set{FunctionName}()
end
```

with

```julia
const FunctionName = Vector{Symbol}

struct FunctionNameSignaturePair
    name::FunctionName
    signature_hash::UInt
end
```

`FunctionNameSignaturePair` looks like `FunctionNameSignaturePair([:Base, :sqrt], UInt(0xb187232b478))`. It contains a "hash of the function signature, minus variable names", i.e. `Base.sqrt(x::Int)::String` and `Base.sqrt(x::Number)` will have different hashes, but `Base.sqrt(x)` and `Base.sqrt(woww)` won't.

### Example for `compute_symbols_state`


```julia

julia> using ExpressionExplorer

julia> compute_symbols_state(:(a = b + c))
SymbolsState(
    references=Set([:b, :c]), 
    assignments=Set([:a]), 
    funccalls=Set([[:+]]), 
    funcdefs=Dict{ExpressionExplorer.FunctionNameSignaturePair, SymbolsState}(), 
    macrocalls=Set{Vector{Symbol}}()
)

julia> compute_symbols_state(:(a = b))
SymbolsState(
    references=Set([:b]), 
    assignments=Set([:a]), 
    funccalls=Set{Vector{Symbol}}(), 
    funcdefs=Dict{ExpressionExplorer.FunctionNameSignaturePair, SymbolsState}(), 
    macrocalls=Set{Vector{Symbol}}()
)

julia> compute_symbols_state(:(a(b) = b + c))
SymbolsState(
    references=Set{Symbol}(), 
    assignments=Set{Symbol}(), 
    funccalls=Set{Vector{Symbol}}(), 
    funcdefs=Dict{ExpressionExplorer.FunctionNameSignaturePair, SymbolsState}(
        ExpressionExplorer.FunctionNameSignaturePair([:a], 0x4e081629cf5e5d05) => 
            SymbolsState(
                references=Set([:c]), 
                assignments=Set{Symbol}(), 
                funccalls=Set([[:+]]), 
                funcdefs=Dict{ExpressionExplorer.FunctionNameSignaturePair, SymbolsState}(), 
                macrocalls=Set{Vector{Symbol}}()
            )
        ), 
    macrocalls=Set{Vector{Symbol}}()
)

```

## Configuration

It is possible to tweak the behaviour of ExpressionExplorer for specific expression types. For example, you can choose to never look inside a `for` expression (not sure why you would want that). In Pluto, we use this to tweak the way `macrocall` expressions are explored, in a way that we did not want to include in this more general package.

You can configure ExpressionExplorer by creating a new subtype:

```julia
struct IgnoreForLoops <: ExpressionExplorer.AbstractExpressionExplorerConfiguration
end
```

Choose an `explore_...` method that you want to overload, and add an overload with `IgnoreForLoops`. You will need to read ExpressionExplorer's source code for this.

```julia
function ExpressionExplorer.explore_inner_scoped(ex::Expr, scopestate::ExpressionExplorer.ScopeState{IgnoreForLoops})::SymbolsState
    if ex.head === :for
        # ignore everything: report that nothing was found here:
        return SymbolsState()
    else
        # the original code:
        innerscopestate = deepcopy(scopestate)
        innerscopestate.inglobalscope = false

        return mapfoldl(a -> explore!(a, innerscopestate), ex.args)
    end
end
```

Then, use the `configuration` keyword argument when using ExpressionExplorer:

```julia
julia> my_expr = quote
    a = b
    for x in z
        w = rrr
    end
end; 

julia> ExpressionExplorer.compute_reactive_node(my_expr; configuration=IgnoreForLoops())
ReactiveNode(Set([:b]), Set([:a]), Set{Symbol}(), Set{FunctionNameSignaturePair}(), Set{Symbol}(), Set{Symbol}())

julia> ExpressionExplorer.compute_reactive_node(my_expr)
ReactiveNode(Set([:b, :rrr, :z]), Set([:a]), Set{Symbol}(), Set{FunctionNameSignaturePair}(), Set{Symbol}(), Set{Symbol}())
```

## Utility functions

The package also includes some utility functions used by Pluto.jl, that might also be useful to other packages.

### `compute_usings_imports`

With `compute_usings_imports` you can extract all `using` or `import` expressions contained in a larger expression.

```julia
julia> ex = quote
           if something
               import A.B: c
           else
               using D
           end
       end
quote;

julia> result = compute_usings_imports(ex);

julia> result.usings
Set{Expr} with 1 element:
  :(using D)

julia> result.imports
Set{Expr} with 1 element:
  :(import A.B: c)
```

This function is used by Pluto's built-in package manager to learn which packages are used in a notebook.

### `can_be_function_wrapped`

```julia
can_be_function_wrapped(ex)::Bool
```

Is this code simple enough that we can wrap it inside a function, and run the function in global scope instead of running the code directly? Look for `Pluto.PlutoRunner.Computer` to learn more.

### `get_rootassignee`
```julia
get_rootassignee(ex)::Union{Symbol,Nothing}
```

If the expression is a (simple) assignemnt at its root, return the assignee as `Symbol`, return `nothing` otherwise.


### `is_toplevel_expr`
```julia
is_toplevel_expr(ex)::Bool
```

Return whether the expression is of the form `Expr(:toplevel, LineNumberNode(..), any)`.


