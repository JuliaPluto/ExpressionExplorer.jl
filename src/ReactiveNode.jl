import .FunctionDependencies

"Every cell is a node in the reactive graph. The nodes/point/vertices are the _cells_, and the edges/lines/arrows are the _dependencies between cells_. In a reactive notebook, these dependencies are the **global variable references and definitions**. (For the mathies: a reactive notebook is represented by a _directed multigraph_. A notebook without reactivity errors is an _acyclic directed multigraph_.) This struct contains the back edges (`references`) and forward edges (`definitions`, `soft_definitions`, `funcdefs_with_signatures`, `funcdefs_without_signatures`) of a single node.

Before 0.12.0, we could have written this struct with just two fields: `references` and `definitions` (both of type `Set{Symbol}`) because we used variable names to form the reactive links. However, to support defining _multiple methods of the same function in different cells_ (https://github.com/fonsp/Pluto.jl/issues/177), we needed to change this. You might want to think about this old behavior first (try it on paper) before reading on.

The essential idea is that edges are still formed by variable names. Simple global variables (`x = 1`) are registered by their name as `Symbol`, but _function definitions_ `f(x::Int) = 5` are sometimes stored in two ways:
- by their name (`f`) as `Symbol`, in `funcdefs_without_signatures`, and
- by their name with its method signature as `FunctionNameSignaturePair`, in `funcdefs_with_signatures`.

The name _without_ signature is most important: it is used to find the reactive dependencies between cells. The name _with_ signature is needed to detect multiple cells that define methods with the _same_ signature (`f(x) = 1` and `f(x) = 2`) - this is illegal. This is why we do not collect `definitions`, `funcdefs_with_signatures` and `funcdefs_without_signatures` onto a single pile: we need them separately for different searches.
"
Base.@kwdef struct ReactiveNode
    references::Set{Symbol} = Set{Symbol}()
    definitions::Set{Symbol} = Set{Symbol}()
    soft_definitions::Set{Symbol} = Set{Symbol}()
    funcdefs_with_signatures::Set{FunctionNameSignaturePair} = Set{FunctionNameSignaturePair}()
    funcdefs_without_signatures::Set{Symbol} = Set{Symbol}()
    macrocalls::Set{Symbol} = Set{Symbol}()
end

function Base.union!(a::ReactiveNode, bs::ReactiveNode...)
	union!(a.references, (b.references for b in bs)...)
	union!(a.definitions, (b.definitions for b in bs)...)
	union!(a.soft_definitions, (b.soft_definitions for b in bs)...)
	union!(a.funcdefs_with_signatures, (b.funcdefs_with_signatures for b in bs)...)
	union!(a.funcdefs_without_signatures, (b.funcdefs_without_signatures for b in bs)...)
	union!(a.macrocalls, (b.macrocalls for b in bs)...)
	return a
end

"Turn a `SymbolsState` into a `ReactiveNode`. The main differences are:
- A `SymbolsState` is a nested structure of function definitions inside function definitions inside... This conversion flattens this structure by merging `SymbolsState`s from defined functions.
- `ReactiveNode` functions as a cache to improve efficiently, by turning the nested structures into multiple `Set{Symbol}`s with fast lookups."
function ReactiveNode(symstate::SymbolsState)
	macrocalls_joined = Set{Symbol}(x.joined for x in symstate.macrocalls)
	result = ReactiveNode(;
		references=Set{Symbol}(symstate.references), 
		definitions=Set{Symbol}(symstate.assignments),
		macrocalls=macrocalls_joined,
	)

	# defined functions are 'exploded' into the cell's reactive node
	for (_, body_symstate) in symstate.funcdefs
		union!(result, ReactiveNode(body_symstate))
	end
	# union!(result, (ReactiveNode(body_symstate) for (_, body_symstate) in symstate.funcdefs)...)

	# now we will add the function names to our edges:
	funccalls = Set{Symbol}(x.joined for x in symstate.funccalls)
	FunctionDependencies.maybe_add_dependent_funccalls!(funccalls)
	union!(result.references, funccalls)

	# Both the first part and the joined paths may be needed for reactivity, see PR #30.
	macrocalls_first_part = Set{Symbol}(first(x.parts) for x in symstate.macrocalls)
	union!(result.references, macrocalls_first_part, macrocalls_joined)

	for (namesig, body_symstate) in symstate.funcdefs
		push!(result.funcdefs_with_signatures, namesig)
		push!(result.funcdefs_without_signatures, namesig.name.joined)

		generated_names = generate_funcnames(namesig.name)
		generated_names_syms = Set{Symbol}(x.joined for x in generated_names)

		# add the generated names so that they are added as soft definitions
		# this means that they will not be used if a cycle is created
		union!(result.soft_definitions, generated_names_syms)

		filter!(!∈(generated_names_syms), result.references) # don't reference defined functions (simulated recursive calls)
	end

	return result
end

# Mot just a method of ReactiveNode because an expression is not necessarily a `Expr`, e.g. `Meta.parse("\"hello!\"") isa String`.
compute_reactive_node(expr::Any; kwargs...) = ReactiveNode(compute_symbols_state(expr; kwargs...))
