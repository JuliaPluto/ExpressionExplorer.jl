@testset "get_rootassignee" begin
    
    g = ExpressionExplorer.get_rootassignee
    
    @test g(:(a = 1)) === :a
    @test g(:(const a = 1)) === Symbol("const a")
    @test g(:(a,b = 1)) === nothing
    @test g(:(a[1] = 1)) === nothing
    
end



@testset "is_toplevel_expr" begin
    
    @test ExpressionExplorer.is_toplevel_expr(Expr(:toplevel, LineNumberNode(-1), :(a = 1)))
    @test !ExpressionExplorer.is_toplevel_expr(:(a = 1))
end

