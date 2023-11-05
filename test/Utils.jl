@testset "get_rootassignee" begin
    
    g = ExpressionExplorer.get_rootassignee
    
    @test g(:(a = 1)) === :a
    @test g(:(const a = 1)) === Symbol("const a")
    @test g(:(a,b = 1)) === nothing
    @test g(:(a[1] = 1)) === nothing
    
end


@testset "can_be_function_wrapped" begin

    c = ExpressionExplorer.can_be_function_wrapped


    @test c(quote
        a = b + C
        if d
            for i = 1:10
                while Y
                end
            end
        end
    end)


    @test c(quote
        map(1:10) do i
            i + 1
        end
    end)


    @test !c(quote
        function x(x)
            X
        end
    end)

    @test !c(quote
        if false
            using Asdf
        end
    end)


end



@testset "is_toplevel_expr" begin
    
    @test ExpressionExplorer.is_toplevel_expr(Expr(:toplevel, LineNumberNode(-1), :(a = 1)))
    @test !ExpressionExplorer.is_toplevel_expr(:(a = 1))
end

