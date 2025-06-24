@testset "ReactiveNode" begin
    rn = ExpressionExplorer.compute_reactive_node(quote
        () -> Date
    end)
    @test :Date ∈ rn.references

    rn = compute_reactive_node(:(Pack))
    @test rn.references == Set([:Pack])

    rn = compute_reactive_node(:(@asdf a[1,k[j]] := log(x[i]/y[j])))
    @test rn.references == Set([Symbol("@asdf")])
    @test rn.macrocalls == Set([Symbol("@asdf")])

    rn = compute_reactive_node(:(Pack.@asdf a[1,k[j]] := log(x[i]/y[j])))
    @test rn.references == Set([:Pack, Symbol("Pack.@asdf")])
    @test rn.macrocalls == Set([Symbol("Pack.@asdf")])
end
