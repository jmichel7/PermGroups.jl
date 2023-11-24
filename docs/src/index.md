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
last_moved(::Perm)
first_moved
preimage
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
reflection_length(::Perm)
mappingPerm
Perm_rowcol
```
# Groups
```@docs
Groups
Group
generators
number_of_generators
one(::Group)
orders_of_generators
ontuples
onsets
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
number_of_conjugacy_classes
position_class
fusion_conjugacy_classes
minimal_words
words(::Group)
transporting_element
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
last_moved(::PermGroup)
in(::Perm,::PermGroup)
on_classes
symmetric_group
onmats
stab_onmats
PermGroups.Perm_onmats
PermGroups.ProdIterator
```
