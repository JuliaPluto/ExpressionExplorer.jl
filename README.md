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

Let's compute the `ReactiveNode` for these two expressions:

```julia
julia> e1 = :(weather = magic() + science);

julia> e2 = :(weather() = magic() + science);
```

First one:

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

Second one, note that `weather` is a function definition, so it does not show up in `r2.definitions`. If you want everything that is defined, you can use `r2.definitions ∪ r2.funcdefs_without_signatures`.

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



## Low-level: `SymbolsState`

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

## Macro calls

ExpressionExplorer ignores the arguments of macro calls. Macros can transform an expression into anything, so the output of ExpressionExplorer for expressions with a macro call is ambiguous. For example, the expression `@time x` contains a *reference* to `x`, while `@gensym x` contains a *definition* of `x`.

In this example, notice that the assignment to `x` and reference to `y` are detected, but `AAA` and `BBB` are ignored, because they happen inside a macro call argument.

```julia
julia> ExpressionExplorer.compute_reactive_node(quote
           x = y
           @time AAA = BBB
       end)
ReactiveNode(Set([Symbol("@time"), :y]), Set([:x]), Set{Symbol}(), Set{FunctionNameSignaturePair}(), Set{Symbol}(), Set([Symbol("@time")]))
```

You can check whether there were any unexplored macro call arguments with the `.macrocalls` field of the `ReactiveNode`, which should be an empty set.

To solve this, you can **macroexpand expressions before giving them to ExpressionExplorer**. For example:

```julia
julia> ExpressionExplorer.compute_reactive_node(macroexpand(Main, quote
           x = y
           @time AAA = BBB
       end))
ReactiveNode(Set([:first, :GC_Diff, :isnothing, :gc_alloc_count, :-, :gc_num, :cumulative_compile_time_ns, :time_ns, :y, :BBB, :print, :cumulative_compile_timing, :time_print, :last, :!]), Set([:AAA, :x]), Set{Symbol}(), Set{FunctionNameSignaturePair}(), Set{Symbol}(), Set{Symbol}())
```

Notice that now, `AAA` and `BBB` are detected, along with functions used inside the `@time` expression.

### Ignoring macros
You can also ignore all macros, and analyse the macro call arguments as if the macro was not there. [Here](https://github.com/JuliaPluto/ExpressionExplorer.jl/issues/21) is a discussion showing how to do it.

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

### `get_rootassignee`
```julia
get_rootassignee(ex)::Union{Symbol,Nothing}
```

If the expression is a (simple) assignment at its root, return the assignee as `Symbol`, return `nothing` otherwise.


### `is_toplevel_expr`
```julia
is_toplevel_expr(ex)::Bool
```

Return whether the expression is of the form `Expr(:toplevel, LineNumberNode(..), any)`.


