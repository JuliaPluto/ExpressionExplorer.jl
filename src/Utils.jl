
"""
```julia
is_toplevel_expr(ex)::Bool
```

Return whether the expression is of the form `Expr(:toplevel, LineNumberNode(..), any)`.
"""
function is_toplevel_expr(ex::Expr)::Bool
    Meta.isexpr(ex, :toplevel, 2) && (ex.args[1] isa LineNumberNode)
end

is_toplevel_expr(::Any)::Bool = false

"""
```julia
get_rootassignee(ex)::Union{Symbol,Nothing}
```

If the expression is a (simple) assignemnt at its root, return the assignee as `Symbol`, return `nothing` otherwise.
"""
function get_rootassignee(ex::Expr, recurse::Bool = true)::Union{Symbol,Nothing}
    if is_toplevel_expr(ex) && recurse
        get_rootassignee(ex.args[2], false)
    elseif Meta.isexpr(ex, :macrocall, 3)
        rooter_assignee = get_rootassignee(ex.args[3], true)
        if rooter_assignee !== nothing
            Symbol(string(ex.args[1]) * " " * string(rooter_assignee))
        else
            nothing
        end
    elseif Meta.isexpr(ex, :const, 1)
        rooter_assignee = get_rootassignee(ex.args[1], false)
        if rooter_assignee !== nothing
            Symbol("const " * string(rooter_assignee))
        else
            nothing
        end
    elseif ex.head == :(=) && ex.args[1] isa Symbol
        ex.args[1]
    else
        nothing
    end
end

get_rootassignee(ex::Any, recuse::Bool = true)::Union{Symbol,Nothing} = nothing


########################


"""
```julia
can_be_function_wrapped(ex)::Bool
```

Is this code simple enough that we can wrap it inside a function, and run the function in global scope instead of running the code directly? Look for `Pluto.PlutoRunner.Computer` to learn more.
"""
function can_be_function_wrapped(x::Expr)
    if x.head === :global || # better safe than sorry
       x.head === :using ||
       x.head === :import ||
       x.head === :export ||
       x.head === :public || # Julia 1.11
       x.head === :module ||
       x.head === :incomplete ||
       # Only bail on named functions, but anonymous functions (args[1].head == :tuple) are fine.
       # TODO Named functions INSIDE other functions should be fine too
       (x.head === :function && !Meta.isexpr(x.args[1], :tuple)) ||
       x.head === :macro ||
       # Cells containing macrocalls will actually be function wrapped using the expanded version of the expression
       # See https://github.com/fonsp/Pluto.jl/pull/1597
       x.head === :macrocall ||
       x.head === :struct ||
       x.head === :abstract ||
       (x.head === :(=) && is_function_assignment(x)) || # f(x) = ...
       (x.head === :call && (x.args[1] === :eval || x.args[1] === :include))
        false
    else
        all(can_be_function_wrapped, x.args)
    end

end

can_be_function_wrapped(x::Any) = true

