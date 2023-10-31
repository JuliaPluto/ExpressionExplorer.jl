# ExpressionExplorer.jl
 
Find all variables referenced and assigned in an expression. This package is used internally by Pluto to find links between cells.

## API

The function `try_compute_symbolreferences` take an expression as argument, and returns a `SymbolsState`.

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
```

## Example

This looks a bit tricky because 

```julia

julia> using ExpressionExplorer

julia> try_compute_symbolreferences(:(a = b + c))
SymbolsState(
    references=Set([:b, :c]), 
    assignments=Set([:a]), 
    funccalls=Set([[:+]]), 
    funcdefs=Dict{ExpressionExplorer.FunctionNameSignaturePair, SymbolsState}(), 
    macrocalls=Set{Vector{Symbol}}()
)

julia> try_compute_symbolreferences(:(a = b))
SymbolsState(
    references=Set([:b]), 
    assignments=Set([:a]), 
    funccalls=Set{Vector{Symbol}}(), 
    funcdefs=Dict{ExpressionExplorer.FunctionNameSignaturePair, SymbolsState}(), 
    macrocalls=Set{Vector{Symbol}}()
)

julia> try_compute_symbolreferences(:(a(b) = b + c))
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