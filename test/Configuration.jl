@testset "Macros and heuristics w/o Pluto" begin
    @test test_expression_explorer(;
        expr=:(@macro import Pkg),
        macrocalls=[Symbol("@macro")],
        definitions=[],
    )
    @test test_expression_explorer(;
        expr=:(@macro Pkg.activate("..")),
        macrocalls=[Symbol("@macro")],
        references=[],
        funccalls=[],
    )
    @test test_expression_explorer(;
        expr=:(@macro Pkg.add("Pluto.jl")),
        macrocalls=[Symbol("@macro")],
        references=[],
        funccalls=[],
    )
    @test test_expression_explorer(;
        expr=:(@macro include("Firebasey.jl")),
        macrocalls=[Symbol("@macro")],
        funccalls=[],
    )
end


include("PlutoConfiguration.jl")
import .PlutoConfigurationSetup

configuration = PlutoConfigurationSetup.PlutoConfiguration()




@testset "Macros and heuristics w/ Pluto" begin
    
    @test test_expression_explorer(;
        expr=:(@macro import Pkg),
        macrocalls=[Symbol("@macro")],
        definitions=[:Pkg],
        configuration,
    )
    @test test_expression_explorer(;
        expr=:(@macro Pkg.activate("..")),
        macrocalls=[Symbol("@macro")],
        references=[:Pkg],
        funccalls=[[:Pkg, :activate]],
        configuration,
    )
    @test test_expression_explorer(;
        expr=:(@macro Pkg.add("Pluto.jl")),
        macrocalls=[Symbol("@macro")],
        references=[:Pkg],
        funccalls=[[:Pkg, :add]],
        configuration,
    )
    @test test_expression_explorer(;
        expr=:(@macro include("Firebasey.jl")),
        macrocalls=[Symbol("@macro")],
        funccalls=[[:include]],
        configuration,
    )
end





@testset "Macros w/ Pluto 1" begin
    # Macros tests are not just in ExpressionExplorer now

    @test testee(:(@time a = 2), [], [], [], [], [Symbol("@time")]; configuration)
    @test testee(:(@f(x; y=z)), [], [], [], [], [Symbol("@f")]; configuration)
    @test testee(:(@f(x, y = z)), [], [], [], [], [Symbol("@f")]) # https://github.com/fonsp/Pluto.jl/issues/252
    @test testee(:(Base.@time a = 2), [], [], [], [], [[:Base, Symbol("@time")]]; configuration)
    # @test_nowarn testee(:(@enum a b = d c), [:d], [:a, :b, :c], [Symbol("@enum")], [])
    # @enum is tested in test/React.jl instead
    @test testee(:(@gensym a b c), [], [:a, :b, :c], [:gensym], [], [Symbol("@gensym")]; configuration)
    @test testee(:(Base.@gensym a b c), [], [:a, :b, :c], [:gensym], [], [[:Base, Symbol("@gensym")]]; configuration)
    @test testee(:(Base.@kwdef struct A; x = 1; y::Int = two; z end), [], [], [], [], [[:Base, Symbol("@kwdef")]]; configuration)
    @test testee(quote "asdf" f(x) = x end, [], [], [], [], [Symbol("@doc")]; configuration)

    # @test testee(:(@bind a b), [], [], [], [], [Symbol("@bind")]; configuration)
    # @test testee(:(PlutoRunner.@bind a b), [], [], [], [], [[:PlutoRunner, Symbol("@bind")]]; configuration)
    # @test_broken testee(:(Main.PlutoRunner.@bind a b), [:b], [:a], [[:Base, :get], [:Core, :applicable], [:PlutoRunner, :create_bond], [:PlutoRunner, Symbol("@bind")]], [], verbose=false; configuration)
    # @test testee(:(let @bind a b end), [], [], [], [], [Symbol("@bind")]; configuration)

    @test testee(:(`hey $(a = 1) $(b)`), [:b], [], [:cmd_gen], [], [Symbol("@cmd")]; configuration)
    # @test testee(:(md"hey $(@bind a b) $(a)"), [:a], [], [[:getindex]], [], [Symbol("@md_str"), Symbol("@bind")]; configuration)
    # @test testee(:(md"hey $(a) $(@bind a b)"), [:a], [], [[:getindex]], [], [Symbol("@md_str"), Symbol("@bind")]; configuration)

    @test testee(:(@asdf a = x1 b = x2 c = x3), [], [], [], [], [Symbol("@asdf")]; configuration) # https://github.com/fonsp/Pluto.jl/issues/670

    @test testee(:(@einsum a[i,j] := x[i]*y[j]), [], [], [], [], [Symbol("@einsum")]; configuration)
    @test testee(:(@tullio a := f(x)[i+2j, k[j]] init=z), [], [], [], [], [Symbol("@tullio")]; configuration)
    @test testee(:(Pack.@asdf a[1,k[j]] := log(x[i]/y[j])), [], [], [], [], [[:Pack, Symbol("@asdf")]]; configuration)


    @test testee(:(html"a $(b = c)"), [], [], [], [], [Symbol("@html_str")]; configuration)
    @test testee(:(md"a $(b = c) $(b)"), [:c], [:b], [:getindex], [], [Symbol("@md_str")]; configuration)
    @test testee(:(md"\* $r"), [:r], [], [:getindex], [], [Symbol("@md_str")]; configuration)
    @test testee(:(md"a \$(b = c)"), [], [], [:getindex], [], [Symbol("@md_str")]; configuration)
    @test testee(:(macro a() end), [], [], [], [
        Symbol("@a") => ([], [], [], [])
    ]; configuration)
    @test testee(:(macro a(b::Int); b end), [], [], [], [
        Symbol("@a") => ([:Int], [], [], [])
    ]; configuration)
    @test testee(:(macro a(b::Int=c) end), [], [], [], [
        Symbol("@a") => ([:Int, :c], [], [], [])
    ]; configuration)
    @test testee(:(macro a(); b = c; return b end), [], [], [], [
        Symbol("@a") => ([:c], [], [], [])
    ]; configuration)
    @test test_expression_explorer(;
        expr=:(@parent @child 10),
        macrocalls=[Symbol("@parent"), Symbol("@child")],
        configuration
    )
    @test test_expression_explorer(;
        expr=:(@parent begin @child 1 + @grandchild 10 end),
        macrocalls=[Symbol("@parent"), Symbol("@child"), Symbol("@grandchild")],
        configuration
    )
    @test testee(macroexpand(Main, :(@noinline f(x) = x)), [], [], [], [
        Symbol("f") => ([], [], [], [])
    ]; configuration)
end


@testset "Macros w/ Pluto" begin
    
    @test testee(:(@bind a b), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get]], [], [Symbol("@bind")]; configuration)
    @test testee(:(PlutoRunner.@bind a b), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get]], [], [[:PlutoRunner, Symbol("@bind")]]; configuration)
    @test_broken testee(:(Main.PlutoRunner.@bind a b), [:b, :PlutoRunner, :Base, :Core], [:a], [[:Base, :get], [:Core, :applicable], [:PlutoRunner, :create_bond], [:PlutoRunner, Symbol("@bind")]], [], verbose=false, configuration)
    @test testee(:(let @bind a b end), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get]], [], [Symbol("@bind")]; configuration)

    @test testee(:(`hey $(a = 1) $(b)`), [:b], [], [:cmd_gen], [], [Symbol("@cmd")]; configuration)
    @test testee(:(md"hey $(@bind a b) $(a)"), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get], :getindex], [], [Symbol("@md_str"), Symbol("@bind")]; configuration)
    @test testee(:(md"hey $(a) $(@bind a b)"), [:a, :b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get], :getindex], [], [Symbol("@md_str"), Symbol("@bind")]; configuration)

    
end