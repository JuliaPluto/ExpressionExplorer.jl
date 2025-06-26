@testset "ReactiveNode" begin
    rn = compute_reactive_node(:(Pack))
    @test rn.references == Set([:Pack])

    rn = ExpressionExplorer.compute_reactive_node(quote
        () -> Date
    end)
    @test :Date ∈ rn.references

    rn = compute_reactive_node(:(@asdf a[1,k[j]] := log(x[i]/y[j])))
    @test rn.references == Set([Symbol("@asdf")])
    @test rn.macrocalls == Set([Symbol("@asdf")])

    rn = compute_reactive_node(:(One.Two.@asdf a[1,k[j]] := log(x[i]/y[j])))
    @test rn.references == Set([:One, Symbol("One.Two.@asdf")])
    @test rn.macrocalls == Set([Symbol("One.Two.@asdf")])
end
