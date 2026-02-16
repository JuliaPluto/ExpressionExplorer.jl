using Aqua, ExplicitImports, ExpressionExplorer

ThisPackage = ExpressionExplorer

@testset "ExplicitImports" begin
    @test ExplicitImports.check_no_implicit_imports(ThisPackage) === nothing
    @test ExplicitImports.check_all_explicit_imports_via_owners(ThisPackage) === nothing
    @static if VERSION >= v"1.11.0"
        @test ExplicitImports.check_all_explicit_imports_are_public(ThisPackage) === nothing
    end
    @test ExplicitImports.check_no_stale_explicit_imports(ThisPackage) === nothing
    @test ExplicitImports.check_all_qualified_accesses_via_owners(ThisPackage) === nothing
    if VERSION >= v"1.12"
        @test ExplicitImports.check_all_qualified_accesses_are_public(ThisPackage) === nothing
    end
    @test ExplicitImports.check_no_self_qualified_accesses(ThisPackage) === nothing
end

@testset "Aqua" begin
    Aqua.test_all(ThisPackage)
end

@testset "UndocumentedNames" begin
    if isdefined(Docs, :undocumented_names) # >=1.11
        @test isempty(filter!(e->e ≠ :try_compute_symbolreferences, Docs.undocumented_names(ThisPackage)))
    end
end
