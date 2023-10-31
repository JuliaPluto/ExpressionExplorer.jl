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








@testset "Macros w/ Pluto" begin
    
    @test testee(:(@bind a b), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get]], [], [Symbol("@bind")]; configuration)
    @test testee(:(PlutoRunner.@bind a b), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get]], [], [[:PlutoRunner, Symbol("@bind")]]; configuration)
    @test_broken testee(:(Main.PlutoRunner.@bind a b), [:b, :PlutoRunner, :Base, :Core], [:a], [[:Base, :get], [:Core, :applicable], [:PlutoRunner, :create_bond], [:PlutoRunner, Symbol("@bind")]], [], verbose=false, configuration)
    @test testee(:(let @bind a b end), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get]], [], [Symbol("@bind")]; configuration)

    @test testee(:(`hey $(a = 1) $(b)`), [:b], [], [:cmd_gen], [], [Symbol("@cmd")]; configuration)
    @test testee(:(md"hey $(@bind a b) $(a)"), [:b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get], :getindex], [], [Symbol("@md_str"), Symbol("@bind")]; configuration)
    @test testee(:(md"hey $(a) $(@bind a b)"), [:a, :b, :PlutoRunner, :Base, :Core], [:a], [[:PlutoRunner, :create_bond], [:Core, :applicable], [:Base, :get], :getindex], [], [Symbol("@md_str"), Symbol("@bind")]; configuration)

    
end