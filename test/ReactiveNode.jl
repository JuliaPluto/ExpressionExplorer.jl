@testset "ReactiveNode" begin
    rn = ExpressionExplorer.compute_reactive_node(quote
        () -> Date
    end)
    @test :Date ∈ rn.references
end