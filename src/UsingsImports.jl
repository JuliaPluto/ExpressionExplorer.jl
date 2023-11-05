
Base.@kwdef struct UsingsImports
    usings::Set{Expr} = Set{Expr}()
    imports::Set{Expr} = Set{Expr}()
end



"""
```julia
compute_usings_imports(ex)::UsingsImports
```

Get the sets of subexpressions like `using Module.Z, SomethingElse` or `import Module` that are contained in this expression.
"""
compute_usings_imports(ex) = compute_usings_imports!(UsingsImports(), ex)

# Performance analysis: https://gist.github.com/fonsp/280f6e883f419fb3a59231b2b1b95cab
"Preallocated version of [`compute_usings_imports`](@ref)."
function compute_usings_imports!(out::UsingsImports, ex::Any)
    if isa(ex, Expr)
        if ex.head == :using
            push!(out.usings, ex)
        elseif ex.head == :import
            push!(out.imports, ex)
        elseif ex.head != :quote
            for a in ex.args
                compute_usings_imports!(out, a)
            end
        end
    end
    out
end



###############



"""
```julia
external_package_names(ex::Union{UsingsImports,Expr})::Set{Symbol}
```

Given `:(using Plots, Something.Else, .LocalModule)`, return `Set([:Plots, :Something])`.
"""
function external_package_names(ex::Expr)::Set{Symbol}
    @assert ex.head == :import || ex.head == :using
    if Meta.isexpr(ex.args[1], :(:))
        external_package_names(Expr(ex.head, ex.args[1].args[1]))
    else
        out = Set{Symbol}()
        for a in ex.args
            if Meta.isexpr(a, :as)
                a = a.args[1]
            end
            if Meta.isexpr(a, :(.))
                if a.args[1] != :(.)
                    push!(out, a.args[1])
                end
            end
        end
        out
    end
end

function external_package_names(x::UsingsImports)::Set{Symbol}
    union!(Set{Symbol}(), Iterators.map(external_package_names, x.usings)..., Iterators.map(external_package_names, x.imports)...)
end



################


function collect_implicit_usings(ex::Expr)
    if is_implicit_using(ex)
        Set{Expr}(Iterators.map(transform_dot_notation, ex.args))
    else
        return Set{Expr}()
    end
end

collect_implicit_usings(usings::Set{Expr}) = mapreduce(collect_implicit_usings, union!, usings; init = Set{Expr}())
collect_implicit_usings(usings_imports::UsingsImports) = collect_implicit_usings(usings_imports.usings)


is_implicit_using(ex::Expr) = Meta.isexpr(ex, :using) && length(ex.args) >= 1 && !Meta.isexpr(ex.args[1], :(:))

function transform_dot_notation(ex::Expr)
    if Meta.isexpr(ex, :(.))
        Expr(:block, ex.args[end])
    else
        ex
    end
end



##################
