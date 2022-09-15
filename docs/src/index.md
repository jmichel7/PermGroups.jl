This package implements permutations (`Perms`), groups (`Groups`) and combines
them to make permutation groups (`PermGroups`). 

It just depends on the package `Combinat`. The modules `Perms` and `Groups`
could be independent packages of their own.

# Permutations
```@docs
Perms
Perm
Perm(::Integer...)
Perm(::AbstractMatrix{<:Integer})
Perm(::AbstractVector, ::AbstractVector)
Perm(::AbstractMatrix, ::AbstractMatrix)
@perm_str
largest_moved_point(::Perm)
smallest_moved_point
permute
sortPerm
randPerm
orbit(::Perm,::Integer)
orbits(::Perm)
Perms.order
cycles(::Perm)
cycletype(::Perm)
support
sign
Base.Matrix(::Perm,n)
restricted(::Perm,::AbstractVector{<:Integer})
reflength(::Perm)
mappingPerm
Perm_rowcol
```
# Groups
```@docs
Groups
Group
gens
ngens
Groups.orbit(::AbstractVector, ::Any)
Groups.orbits(::Group, ::Any)
elements(::Group)
transversal
words_transversal
centralizer
center
stabilizer
normalizer
word(::Group,::Any)
comm
length(::Group)
classreps(::Group)
conjugacy_classes
conjugacy_class
nconjugacy_classes
position_class
fusion_conjugacy_classes
minimal_words
words(::Group)
transporting_elt
intersect(::Group,::Group)
rand(::Group)
isabelian
iscyclic
istrivial
Hom
kernel
Coset
NormalCoset
```
# Permutation groups
```@docs
PermGroups
largest_moved_point(::PermGroup)
base
centralizers
transversals
in(::Perm,::PermGroup)
on_classes
symmetric_group
onmats
stab_onmats
Perm_onmats
```
