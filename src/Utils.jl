
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

If the expression is a (simple) assignment at its root, return the assignee as `Symbol`, return `nothing` otherwise.
"""
function get_rootassignee(ex::Expr, recurse::Bool = true)::Union{Symbol,Nothing}
    if is_toplevel_expr(ex) && recurse
        get_rootassignee(ex.args[2], false)
    elseif Meta.isexpr(ex, :macrocall, 3)
        rooter_assignee = get_rootassignee(ex.args[3], true)
        if rooter_assignee !== nothing
            Symbol(string(ex.args[1]), " ", string(rooter_assignee))
        else
            nothing
        end
    elseif Meta.isexpr(ex, :const, 1)
        rooter_assignee = get_rootassignee(ex.args[1], false)
        if rooter_assignee !== nothing
            Symbol("const ", string(rooter_assignee))
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
