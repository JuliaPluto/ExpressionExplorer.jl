module PlutoConfigurationSetup

using ..ExpressionExplorer
using ..ExpressionExplorer: ScopeState

module PlutoRunner

using Markdown

function create_bond(args...)
end


initial_value_getter_ref = Ref(nothing)


macro bind(def, element)    
	if def isa Symbol
		quote
			local el = $(esc(element))
			global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : $(initial_value_getter_ref)[](el)
			PlutoRunner.create_bond(el, $(Meta.quot(def)), nothing)
		end
	else
		:(throw(ArgumentError("""\nMacro example usage: \n\n\t@bind my_number html"<input type='range'>"\n\n""")))
	end
end

end


import .PlutoRunner




struct PlutoConfiguration <: ExpressionExplorer.AbstractExpressionExplorerConfiguration
end



"""
Uses `cell_precedence_heuristic` to determine if we need to include the contents of this macro in the symstate.
This helps with things like a Pkg.activate() that's in a macro, so Pluto still understands to disable nbpkg.
"""
function macro_has_special_heuristic_inside(; symstate::SymbolsState, expr::Expr)::Bool
    blub = union(
        symstate.references,
        symstate.assignments,
        ExpressionExplorer.join_funcname_parts.(symstate.funccalls),
    )
    
    yup = ["Pkg", "include"]
    
    return any(blub) do s
        any(yup) do z
            occursin(z, String(s))
        end
    end
end





const can_macroexpand_no_bind = Set(Symbol.(["@md_str", "Markdown.@md_str", "@gensym", "Base.@gensym", "@enum", "Base.@enum", "@assert", "Base.@assert", "@cmd"]))
const can_macroexpand = can_macroexpand_no_bind ∪ Set(Symbol.(["@bind", "PlutoRunner.@bind"]))

"""
If the macro is **known to Pluto**, expand or 'mock expand' it, if not, return the expression. Macros from external packages are not expanded, this is done later in the pipeline. See https://github.com/fonsp/Pluto.jl/pull/1032
"""
function maybe_macroexpand_pluto(ex::Expr; recursive::Bool=false, expand_bind::Bool=true)
    result::Expr = if ex.head === :macrocall
        funcname = ExpressionExplorer.split_funcname(ex.args[1])
        funcname_joined = ExpressionExplorer.join_funcname_parts(funcname)

        if funcname_joined ∈ (expand_bind ? can_macroexpand : can_macroexpand_no_bind)
            macroexpand(PlutoRunner, ex; recursive=false)::Expr
        else
            ex
        end
    else
        ex
    end

    if recursive
        # Not using broadcasting because that is expensive compilation-wise for `result.args::Any`.
        expanded = Any[]
        for arg in result.args
            ex = maybe_macroexpand_pluto(arg; recursive, expand_bind)
            push!(expanded, ex)
        end
        return Expr(result.head, expanded...)
    else
        return result
    end
end

maybe_macroexpand_pluto(ex::Any; kwargs...) = ex







function ExpressionExplorer.explore_macrocall!(ex::Expr, scopestate::ScopeState{PlutoConfiguration})
    # Early stopping, this expression will have to be re-explored once
    # the macro is expanded in the notebook process.
    macro_name = ExpressionExplorer.split_funcname(ex.args[1])
    symstate = SymbolsState(macrocalls = Set{FunctionName}([macro_name]))

    # Because it sure wouldn't break anything,
    # I'm also going to blatantly assume that any macros referenced in here...
    # will end up in the code after the macroexpansion 🤷‍♀️
    # "You should make a new function for that" they said, knowing I would take the lazy route.
    for arg in ex.args[begin+1:end]
        macro_symstate = ExpressionExplorer.explore!(arg, ScopeState(scopestate.configuration))

        # Also, when this macro has something special inside like `Pkg.activate()`,
        # we're going to treat it as normal code (so these heuristics trigger later)
        # (Might want to also not let this to @eval macro, as an extra escape hatch if you
        #    really don't want pluto to see your Pkg.activate() call)
        if arg isa Expr && macro_has_special_heuristic_inside(symstate = macro_symstate, expr = arg)
            union!(symstate, macro_symstate)
        else
            union!(symstate, SymbolsState(macrocalls = macro_symstate.macrocalls))
        end
    end

    # Some macros can be expanded on the server process
    if ExpressionExplorer.join_funcname_parts(macro_name) ∈ can_macroexpand
        new_ex = maybe_macroexpand_pluto(ex)
        union!(symstate, ExpressionExplorer.explore!(new_ex, scopestate))
    end

    return symstate
end

end


