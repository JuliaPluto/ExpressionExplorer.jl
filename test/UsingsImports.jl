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

@testset "Extracting `using` and `import`" begin
    expr = quote
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
    result = ExpressionExplorer.compute_usings_imports(expr)
    @test result.usings == [
        :(using A),
        :(using .C: r),
    ]
    @test result.imports == [
        :(import B),
        :(import ..D.E: f, g),
        :(import H.I, J, K.L),
    ]

    @test ExpressionExplorer.external_package_names(result) == Set{Symbol}([
        :A, :B, :H, :J, :K
    ])

    @test ExpressionExplorer.external_package_names(:(using Plots, Something.Else, .LocalModule)) == Set([:Plots, :Something])
    @test ExpressionExplorer.external_package_names(:(import Plots.A: b, c)) == Set([:Plots])

    @test ExpressionExplorer.external_package_names(Meta.parse("import Foo as Bar, Baz.Naz as Jazz")) == Set([:Foo, :Baz])
end

@testset "`using` and `import` in global scope" begin
    expr = quote
        using A
        module MyModule
            using A
            using B
        end
        using .MyModule
    end
    result = ExpressionExplorer.compute_usings_imports(expr)
    @test result.usings == [
        :(using A),
        :(using A),
        :(using B),
        :(using .MyModule),
    ]
    @test result.usings_isglobal == [true, false, false, true]
end
