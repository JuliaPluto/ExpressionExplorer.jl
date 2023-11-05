
@testset "Usings imports" begin
    using_test_expr = quote
        using A
        import B
        if x
            using .C: r
            import ..D.E: f, g
        else
            import H.I, J, K.L
        end
        
        quote
            using Nonono
        end
    end
    
    
    r = ExpressionExplorer.compute_usings_imports(using_test_expr)
    @test sort(string.(r.usings)) == ["using .C: r", "using A"]
    @test sort(string.(r.imports)) == ["import ..D.E: f, g", "import B", "import H.I, J, K.L"]
    
    r = ExpressionExplorer.compute_usings_imports(123)
    @test length(r.usings) == 0
    @test length(r.imports) == 0
    
    
end