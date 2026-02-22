"""
    ExpressionExplorer

$(read(joinpath(@__DIR__, "..", "README.md"), String))
"""
module ExpressionExplorer

export compute_symbols_state,
    compute_reactive_node,
    ReactiveNode,
    SymbolsState,
    FunctionName,
    FunctionNameSignaturePair,
    compute_usings_imports,
    UsingsImports,
    ScopeState,
    external_package_names,
    is_function_assignment,
    split_funcname



include("./explore.jl")
include("./UsingsImports.jl")
include("./Utils.jl")
include("./FunctionDependencies.jl")
include("./ReactiveNode.jl")

end
