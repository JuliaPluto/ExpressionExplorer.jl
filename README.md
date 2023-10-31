# ExpressionExplorer.jl
 
Find all variables referenced and assigned in an expression. This package is used internally by Pluto to find links between cells.

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


# Example

Tadaaa

```julia
julia> # the two expressions that we will use in this example:

julia> e1 = :(weather = magic() + science);

julia> e2 = :(weather() = magic() + science);

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



## Example of low-level API


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