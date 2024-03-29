
using ExpressionExplorer
using Test


const ObjectID = typeof(objectid("hello computer"))

function Base.show(io::IO, s::SymbolsState)
    print(io, "SymbolsState([")
    join(io, s.references, ", ")
    print(io, "], [")
    join(io, s.assignments, ", ")
    print(io, "], [")
    join(io, s.funccalls, ", ")
    print(io, "], [")
    if isempty(s.funcdefs)
        print(io, "]")
    else
        println(io)
        for (k, v) in s.funcdefs
            print(io, "    ", k, ": ", v)
            println(io)
        end
        print(io, "]")
    end
    if !isempty(s.macrocalls)
        print(io, "], [")
        print(io, s.macrocalls)
        print(io, "])")
    else
        print(io, ")")
    end
end

"Calls `ExpressionExplorer.compute_symbolreferences` on the given `expr` and test the found SymbolsState against a given one, with convient syntax.

# Example

```jldoctest
julia> @test testee(:(
    begin
        a = b + 1
        f(x) = x / z
    end),
    [:b, :+], # 1st: expected references
    [:a, :f], # 2nd: expected definitions
    [:+],     # 3rd: expected function calls
    [
        :f => ([:z, :/], [], [:/], [])
    ])        # 4th: expected function definitions, with inner symstate using the same syntax
true
```
"
function testee(expr::Any, expected_references, expected_definitions, expected_funccalls, expected_funcdefs, expected_macrocalls = []; verbose::Bool=true)
    expected = easy_symstate(expected_references, expected_definitions, expected_funccalls, expected_funcdefs, expected_macrocalls)

    original_hash = expr_hash(expr)
    result = ExpressionExplorer.compute_symbolreferences(expr)
    # should not throw:
    ReactiveNode(result)
    
    new_hash = expr_hash(expr)
    if original_hash != new_hash
        error("\n== The expression explorer modified the expression. Don't do that! ==\n")
    end

    # Anonymous function are given a random name, which looks like anon67387237861123
    # To make testing easier, we rename all such functions to anon
    new_name(fn::FunctionName) = FunctionName(map(new_name, fn.parts)...)
    new_name(sym::Symbol) = startswith(string(sym), "__ExprExpl_anon__") ? :anon : sym

    result.assignments = Set(new_name.(result.assignments))
    result.funcdefs = let
        newfuncdefs = Dict{FunctionNameSignaturePair,SymbolsState}()
        for (k, v) in result.funcdefs
            union!(newfuncdefs, Dict(FunctionNameSignaturePair(new_name(k.name), hash("hello")) => v))
        end
        newfuncdefs
    end

    if verbose && expected != result
        println()
        println("FAILED TEST")
        println(expr)
        println()
        dump(expr, maxdepth=20)
        println()
        @show expected
        resulted = result
        @show resulted
        println()
    end
    return expected == result
end




expr_hash(e::Expr) = objectid(e.head) + mapreduce(p -> objectid((p[1], expr_hash(p[2]))), +, enumerate(e.args); init=zero(ObjectID))
expr_hash(x) = objectid(x)




"""
Like `testee` but actually a convenient syntax
"""
function test_expression_explorer(; expr, references=[], definitions=[], funccalls=[], funcdefs=[], macrocalls=[])
    testee(expr, references, definitions, funccalls, funcdefs, macrocalls)
end

function easy_symstate(expected_references, expected_definitions, expected_funccalls, expected_funcdefs, expected_macrocalls = [])
    array_to_set(array) = map(array) do k
        new_k = FunctionName(k)
        return new_k
    end |> Set
    new_expected_funccalls = array_to_set(expected_funccalls)
    
    new_expected_funcdefs = map(expected_funcdefs) do (k, v)
        new_k = FunctionName(k)
        new_v = v isa SymbolsState ? v : easy_symstate(v...)
        return FunctionNameSignaturePair(new_k, hash("hello")) => new_v
    end |> Dict

    new_expected_macrocalls = array_to_set(expected_macrocalls)

    SymbolsState(Set(expected_references), Set(expected_definitions), new_expected_funccalls, new_expected_funcdefs, new_expected_macrocalls)
end
