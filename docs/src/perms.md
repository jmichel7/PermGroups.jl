# Permutations (Perms)
```@docs
Perms
Perm
Perm{T}(x::Vararg{T1,N};degree=0)where {T<:Integer,T1<:Integer,N}
Perm(::AbstractMatrix{<:Integer})
Perm(::AbstractVector, ::AbstractVector)
Perm(::AbstractMatrix, ::AbstractMatrix)
@perm_str
last_moved(a::Perm{T}) where T
first_moved(a::Perm{T}) where T
perm
preimage
invpermute
sortPerm
randPerm
orbit(::Perm,::Integer)
orbits(::Perm,::AbstractVector{<:Integer})
Perms.order(::Perm)
cycles(::Perm)
cycletype(::Perm)
support
sign
Base.Matrix(::Perm,n)
restricted(::Perm,::AbstractVector{<:Integer})
reflection_length(::Perm)
mappingPerm
Perm_rowcol
```
