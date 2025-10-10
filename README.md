
This package implements permutations (`Perms`), groups (`Groups`) and combines them to make permutation groups (`PermGroups`). 


It just depends on the package `Combinat`. The modules `Perms` and `Groups` could be independent packages of their own.


<a id='Permutations'></a>

<a id='Permutations-1'></a>

# Permutations

<a id='PermGroups.Perms' href='#PermGroups.Perms'>#</a>
**`PermGroups.Perms`** &mdash; *Module*.



This package implements permutations and some functions of them. It depends only  on  the  package  `Combinat`.

This  package  follows  the  design  of  permutations  in the GAP language. `Perm`s  are permutations  of the  set `1:n`,  represented internally  as a vector  of `n`  integers holding  the images  of `1:n`.  The integer `n` is called  the degree  of the  permutation. In  this package,  as in  GAP (and contrary  to the philosophy of Magma or the package `Permutations.jl`), two permutations of different  degrees  can  be  multiplied (the result has the larger  degree). Two permutations  are equal if  and only if  they move the same points in the same way, so two permutations of different degree can be equal; the degree is thus an implementation detail so usually it should not be used. One should rather use the function `last_moved`.

This  design makes it  easy to multiply  permutations coming from different groups, like a group and one of its subgroups. It has a negligible overhead compared to the design where the degree is fixed.

The default constructor for a permutation uses the list of images of `1:n`, like  `Perm([2,3,1,5,4,6])`.  Often  it  is  more  convenient  to use cycle decompositions:    the   above   permutation    has   cycle   decomposition `(1,2,3)(4,5)`    thus   can   be    written   `Perm(1,2,3)*Perm(4,5)`   or `perm"(1,2,3)(4,5)"`  (this last form  can parse a  permutation coming from GAP  or the default printing at the REPL).  The list of images of `1:n` can be  recovered from the permutation by  the function `perm`; note that equal permutations  with different degrees will  have different `perm`. Note that the  default constructor  tests the  validity of  the input  by calling the `julia` function `isperm`. To have a faster constructor which does not test the input, use the keyword argument `check=false`.

The  complete type of a permutation  is `Perm{T}` where `T<:Integer`, where `Vector{T}`  is the type of the vector which holds the image of `1:n`. This can be used to save space or time. For instance `Perm{UInt8}(1,2,3)` can be used for Weyl groups of rank≤8 since they permute at most 240 roots. If `T` is  not specified we take it to be  `Int16` since this is a good compromise between   speed,  compactness  and  possible  size  of  `n`.  One  can  mix permutations of different integer types; they are promoted to the wider one when multiplying.

**Examples of operations with permutations**

```julia-repl
julia> a=Perm(1,2,3)
(1,2,3)

julia> perm(a)
3-element Vector{Int16}:
 2
 3
 1

julia> a==Perm(perm(a))
true

julia> b=Perm(1,2,3,4)
(1,2,3,4)

julia> a*b     # product
(1,3,2,4)

julia> inv(a)  # inverse
(1,3,2)

julia> a/b     # quotient  a*inv(b)
(3,4)

julia> a\b     # left quotient inv(a)*b
(1,4)

julia> a^b     # conjugation inv(b)*a*b
(2,3,4)

julia> b^2     # square
(1,3)(2,4)

julia> 1^a     # image by a of point 1
2

julia> one(a)  # trivial permutation
()

julia> sign(a) # signature of permutation
1

julia> order(a) # order (least trivial power) of permutation
3

julia> last_moved(a)
3

julia> first_moved(a)
1

julia> Perm{Int8}(a) # convert a to Perm{Int8}
Perm{Int8}: (1,2,3)

julia> Matrix(b)  # permutation matrix of b
4×4 Matrix{Bool}:
 0  1  0  0
 0  0  1  0
 0  0  0  1
 1  0  0  0
```

```julia-rep1
julia> randPerm(10) # random permutation of 1:10
(1,8,4,2,9,7,5,10,3,6)
```

`Perm`s have methods `copy`, `hash`, `==`, so they can be keys in hashes or elements  of sets; two permutations are equal  if they move the same points to  the same images. They have methods `cmp`, `isless` (lexicographic order on   moved  points)  so  they  can  be  sorted.  `Perm`s  are  scalars  for broadcasting.

Other  methods on  permutations are  `cycles, cycletype, reflection_length, mappingPerm, restricted, support, sortPerm, Perm_rowcol, preimage, randPerm`.

No  method is given in  this package to enumerate  `Perm`s; you can use the method  `permutations` from `Combinat`,  iterate `Combinat.Permutations` or iterate the elements of `symmetric_group` from `PermGroups`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L1-L121' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm' href='#PermGroups.Perms.Perm'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Type*.



`struct Perm{T<:Integer}`

A  Perm represents a permutation  of the set `1:n`  and is implemented by a `struct`  with one field,  a `Vector{T}` holding  the images of `1:n`. When showing  a `Perm` at the REPL, the cycle decomposition is displayed as well as the type if it is not `Int16`. The default constructor checks the input, unless the keyword argument `check=false` is given.

```julia-repl
julia> Perms.Perm_(UInt8[1,3,2,4])
Perm{UInt8}: (2,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L135-L148' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Union{NTuple{N, T1}, Tuple{N}, Tuple{T1}, Tuple{T}} where {T<:Integer, T1<:Integer, N}' href='#PermGroups.Perms.Perm-Union{NTuple{N, T1}, Tuple{N}, Tuple{T1}, Tuple{T}} where {T<:Integer, T1<:Integer, N}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(x::Integer...)where T<:Integer`

returns  a cycle.  For example  `Perm{Int8}(1,2,3)` constructs the cycle `(1,2,3)` as a `Perm{Int8}`. If omitted `{T}` is taken to be `{Int16}`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L181-L186' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Tuple{AbstractMatrix{<:Integer}}' href='#PermGroups.Perms.Perm-Tuple{AbstractMatrix{<:Integer}}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(x::Integer...)where T<:Integer`

returns  a cycle.  For example  `Perm{Int8}(1,2,3)` constructs the cycle `(1,2,3)` as a `Perm{Int8}`. If omitted `{T}` is taken to be `{Int16}`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L181-L186' class='documenter-source'>source</a><br>


`Perm{T}(m::AbstractMatrix)` If  `m` is a  permutation matrix, returns  the corresponding permutation of type `T`. If omitted, `T` is taken to be `Int16`.

```julia-repl
julia> Perm([0 1 0;0 0 1;1 0 0])
(1,2,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L269-L278' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Tuple{AbstractVector, AbstractVector}' href='#PermGroups.Perms.Perm-Tuple{AbstractVector, AbstractVector}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(l::AbstractVector,l1::AbstractVector)`

returns  `p`, a  `Perm{T}`, such  that `invpermute(l1,p)==l`  if such a `p` exists;  returns `nothing`  otherwise. If  not given  `{T}` is  taken to be `{Int16}`. Needs the `eltype` of `l` and `l1` to be sortable.

```julia-repl
julia> Perm([0,2,4],[4,0,2])
(1,3,2)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L757-L768' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Tuple{AbstractMatrix, AbstractMatrix}' href='#PermGroups.Perms.Perm-Tuple{AbstractMatrix, AbstractMatrix}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(m::AbstractMatrix,m1::AbstractMatrix;dims=1)`

returns  `p`, a `Perm{T}`, which invpermutes the  rows of `m1` (the columns of `m1`  if `dims=2`, simultaneously the rows  and columns if `dims=(1,2)`) to bring  them  to  those  of  `m`,  if  such  a `p` exists; returns `nothing` otherwise.  If not given `{T}` is taken to be `{Int16}`. Needs the elements of `m` and `m1` to be sortable.

```julia-repl
julia> Perm([0 1 0;0 0 1;1 0 0],[1 0 0;0 1 0;0 0 1];dims=1)
(1,3,2)

julia> Perm([0 1 0;0 0 1;1 0 0],[1 0 0;0 1 0;0 0 1];dims=2)
(1,2,3)

julia> n=(1:30)'.*(1:30).%15;

julia> m=onmats(n,Perm(1,5,2,8,12,4,7)*Perm(3,9,11,6));

julia> Perm(m,n,dims=(1,2))
(1,5,2,8,12,4,7)(3,9,11,6)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L900-L923' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.@perm_str' href='#PermGroups.Perms.@perm_str'>#</a>
**`PermGroups.Perms.@perm_str`** &mdash; *Macro*.



@perm"..."

makes a `Perm{Int16}` from a string; allows GAP-style `perm"(1,2)(5,6,7)(4,9)"`. If the cycle decomposition is preceded by `"Perm{T}:"` the constructed  permutation is of type `T`.

```julia-repl
perm"Perm{UInt8}:(1,2)(3,4)"
Perm{UInt8}: (1,2)(3,4)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L210-L221' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.last_moved-Tuple{Perm}' href='#PermGroups.Perms.last_moved-Tuple{Perm}'>#</a>
**`PermGroups.Perms.last_moved`** &mdash; *Method*.



`last_moved(a::Perm)` is the largest integer moved by a


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L340' class='documenter-source'>source</a><br>


`last_moved(G::PermGroup)` the largest moved point by any `g∈ G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L101' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.first_moved' href='#PermGroups.Perms.first_moved'>#</a>
**`PermGroups.Perms.first_moved`** &mdash; *Function*.



`first_moved(a::Perm)` is the smallest integer moved by a


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L346' class='documenter-source'>source</a><br>


`first_moved(G::PermGroup)` the smallest moved point by any `g∈ G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L108' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.perm' href='#PermGroups.Perms.perm'>#</a>
**`PermGroups.Perms.perm`** &mdash; *Function*.



`perm(p::Perm)` returns the data field of a `Perm`.

```julia-repl
julia> perm(Perm(2,3;degree=4))
4-element Vector{Int16}:
 1
 3
 2
 4
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L162-L172' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.preimage' href='#PermGroups.Perms.preimage'>#</a>
**`PermGroups.Perms.preimage`** &mdash; *Function*.



`preimage(i::Integer,p::Perm)` or `i/p`

the  preimage of `i` by `p` (same as  image of `i` by `inv(p)` but does not need computing the inverse)."


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L394-L399' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.invpermute' href='#PermGroups.Perms.invpermute'>#</a>
**`PermGroups.Perms.invpermute`** &mdash; *Function*.



`invpermute(l::AbstractVector,p::Perm)`

returns  `l` invpermuted by  `p`, a vector  `r` of same  length as `l` such that `r[i^p]==l[i]` for `i` in `eachindex(l)`. This function corresponds to the  GAP function  `Permuted`, but  we changed  the name  to fit  the Julia conventions since `invpermute(l,p)==invpermute!(copy(l),perm(p))`.

```julia-repl
julia> invpermute([5,4,6],Perm(1,2,3))
3-element Vector{Int64}:
 6
 5
 4
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L427-L442' class='documenter-source'>source</a><br>


`invpermute(m::AbstractMatrix, p1::Perm,p2::Perm)`

invpermutes the rows of `m` by `p1` and the columns of `m` by `p2`.

```julia-repl
julia> m=reshape(1:9,3,3)
3×3 reshape(::UnitRange{Int64}, 3, 3) with eltype Int64:
 1  4  7
 2  5  8
 3  6  9

julia> invpermute(m,Perm(1,2),Perm(2,3))
3×3 Matrix{Int64}:
 2  8  5
 1  7  4
 3  9  6
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L453-L471' class='documenter-source'>source</a><br>


`invpermute(m::AbstractMatrix,p::Perm;dims=1)`

invpermutes  by `p` the rows, columns or  both of the matrix `m` depending on the value of `dims`.

```julia-repl
julia> m=reshape(1:9,3,3)
3×3 reshape(::UnitRange{Int64}, 3, 3) with eltype Int64:
 1  4  7
 2  5  8
 3  6  9

julia> p=Perm(1,2,3)
(1,2,3)

julia> invpermute(m,p)
3×3 Matrix{Int64}:
 3  6  9
 1  4  7
 2  5  8

julia> invpermute(m,p;dims=2)
3×3 Matrix{Int64}:
 7  1  4
 8  2  5
 9  3  6

julia> invpermute(m,p;dims=(1,2))
3×3 Matrix{Int64}:
 9  3  6
 7  1  4
 8  2  5
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L480-L514' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.sortPerm' href='#PermGroups.Perms.sortPerm'>#</a>
**`PermGroups.Perms.sortPerm`** &mdash; *Function*.



for convenience: `sortPerm(a)=Perm(sortperm(a))`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L356' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.randPerm' href='#PermGroups.Perms.randPerm'>#</a>
**`PermGroups.Perms.randPerm`** &mdash; *Function*.



`randPerm([T,]n::Integer)` a random permutation of `1:n` of type `T`. If omitted `T` is taken to be `Int16`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L360-L363' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbit-Tuple{Perm, Integer}' href='#PermGroups.Perms.orbit-Tuple{Perm, Integer}'>#</a>
**`PermGroups.Perms.orbit`** &mdash; *Method*.



`orbit(p::Perm,i::Integer)` returns the orbit of `p` on `i`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L524-L526' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbits-Tuple{Perm}' href='#PermGroups.Perms.orbits-Tuple{Perm}'>#</a>
**`PermGroups.Perms.orbits`** &mdash; *Method*.



`orbits(G::PermGroup)` the orbits of `G` on its moved points.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L115' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.order' href='#PermGroups.Perms.order'>#</a>
**`PermGroups.Perms.order`** &mdash; *Function*.



`order(G::Group)` the number of elements of `G`.

`order(T<:Integer,G::Group)` do the computation with the integer type `T`   (useful when a `BigInt` is needed to hold the result).


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L605-L610' class='documenter-source'>source</a><br>


`order(a)` the smallest integer `i≥1` such that `isone(a^i)`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L695' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.cycles-Tuple{Perm}' href='#PermGroups.Perms.cycles-Tuple{Perm}'>#</a>
**`PermGroups.Perms.cycles`** &mdash; *Method*.



`cycles(a::Perm)` returns the cycles of `a`

**Example**

```julia-repl
julia> cycles(Perm(1,2)*Perm(4,5))
2-element Vector{Vector{Int16}}:
 [1, 2]
 [4, 5]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L571-L580' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.cycletype-Tuple{Perm}' href='#PermGroups.Perms.cycletype-Tuple{Perm}'>#</a>
**`PermGroups.Perms.cycletype`** &mdash; *Method*.



`cycletype(a::Perm,domain::AbstractVector{<:Integer};trivial=true)`

`domain`  should be  a union  of orbits  of `a`.  Returns the  partition of `length(domain)`  associated to the conjugacy class of `a` in the symmetric group  of `domain`, with ones removed if `trivial=false` (in which case the partition does not depend on `domain` but just on `support(a)`)

`cycletype(a::Perm)`

returns `cycletype(a,1:last_moved(a);trivial=false)`

**Example**

```julia-repl
julia> cycletype(Perm(1,2)*Perm(4,5))
2-element Vector{Int64}:
 2
 2

julia> cycletype(Perm(1,2)*Perm(4,5),1:5)
3-element Vector{Int64}:
 2
 2
 1

julia> cycletype(Perm(1,2)*Perm(4,5),1:6)
4-element Vector{Int64}:
 2
 2
 1
 1
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L627-L659' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.support' href='#PermGroups.Perms.support'>#</a>
**`PermGroups.Perms.support`** &mdash; *Function*.



`support(a::Perm)` is the sorted list of all points moved by `a`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L352' class='documenter-source'>source</a><br>

<a id='Base.sign' href='#Base.sign'>#</a>
**`Base.sign`** &mdash; *Function*.



`sign(p::Perm)` is the signature of  the permutation `p`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L690' class='documenter-source'>source</a><br>

<a id='Base.Matrix-Tuple{Perm, Any}' href='#Base.Matrix-Tuple{Perm, Any}'>#</a>
**`Base.Matrix`** &mdash; *Method*.



`Matrix(p::Perm,n=last_moved(p))` the  permutation matrix  for `p`  operating on  `n` points.

```julia-repl
julia> Matrix(Perm(2,3,4),5)
5×5 Matrix{Bool}:
 1  0  0  0  0
 0  0  1  0  0
 0  0  0  1  0
 0  1  0  0  0
 0  0  0  0  1
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L253-L266' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.restricted-Tuple{Perm, AbstractVector{<:Integer}}' href='#PermGroups.Perms.restricted-Tuple{Perm, AbstractVector{<:Integer}}'>#</a>
**`PermGroups.Perms.restricted`** &mdash; *Method*.



`restricted(p::Perm,domain::AbstractVector{<:Integer})`

`domain` should be a union of orbits of `p`; returns `p` restricted to `domain`

```julia-repl
julia> restricted(Perm(1,2)*Perm(3,4),3:4)
(3,4)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L693-L702' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.reflection_length-Tuple{Perm}' href='#PermGroups.Perms.reflection_length-Tuple{Perm}'>#</a>
**`PermGroups.Perms.reflection_length`** &mdash; *Method*.



`reflection_length(p::Perm)` or `reflength`

gives  the  "reflection  length"  of  `p`  (when the symmetric group on `n` points to which `p` belongs is interpreted as a reflection group on a space of  dimension `n`), that is, the  minimum number of transpositions of which `p` is the product.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L678-L685' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.mappingPerm' href='#PermGroups.Perms.mappingPerm'>#</a>
**`PermGroups.Perms.mappingPerm`** &mdash; *Function*.



`mappingPerm([::Type{T},]a::AbstractVector{<:Integer})`

given  a list  of positive  integers without  repetition `a`, this function finds  a  `Perm{T}`  `p`  such  that  `sort(a).^p==a`.  This can be used to translate  between arrangements and `Perm`s. If  omitted `T` is taken to be `Int16`.

```julia-repl
julia> p=mappingPerm([6,7,5])
(5,6,7)

julia> (5:7).^p
3-element Vector{Int64}:
 6
 7
 5
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L709-L727' class='documenter-source'>source</a><br>


`mappingPerm([::Type{T},]a,b)`

given  two lists of positive integers  without repetition `a` and `b`, this function finds a `Perm{T}` `p` such that `a.^p==b`. If omitted `T` is taken to be `Int16`.

```julia-repl
julia> mappingPerm([1,2,5,3],[2,3,4,6])
(1,2,3,6,5,4)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L736-L747' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm_rowcol' href='#PermGroups.Perms.Perm_rowcol'>#</a>
**`PermGroups.Perms.Perm_rowcol`** &mdash; *Function*.



`Perm_rowcol(m1::AbstractMatrix, m2::AbstractMatrix)`

whether `m1` can be obtained from `m2` by row/col permutations.

`m1` and `m2` should be rectangular matrices of the same size. The function returns a `Tuple` of permutations `(p1,p2)` such that `invpermute(m2,p1,p2)==m1` if such permutations exist, `nothing` otherwise. The `eltype` of `m1` and `m2` must be sortable.

```julia-repl
julia> a=[1 1 1 -1 -1; 2 0 -2 0 0; 1 -1 1 -1 1; 1 1 1 1 1; 1 -1 1 1 -1]
5×5 Matrix{Int64}:
 1   1   1  -1  -1
 2   0  -2   0   0
 1  -1   1  -1   1
 1   1   1   1   1
 1  -1   1   1  -1

julia> b=[1 -1 -1 1 1; 1 1 -1 -1 1; 1 -1 1 -1 1; 2 0 0 0 -2; 1 1 1 1 1]
5×5 Matrix{Int64}:
 1  -1  -1   1   1
 1   1  -1  -1   1
 1  -1   1  -1   1
 2   0   0   0  -2
 1   1   1   1   1

julia> p1,p2=Perm_rowcol(a,b)
((1,3,5,4,2), (3,4,5))

julia> invpermute(b,p1,p2)==a
true
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L777-L810' class='documenter-source'>source</a><br>


<a id='Groups'></a>

<a id='Groups-1'></a>

# Groups

<a id='PermGroups.Groups' href='#PermGroups.Groups'>#</a>
**`PermGroups.Groups`** &mdash; *Module*.



This module gives some basic functionality on groups.

`Group`  is  an  abstract  type,  but  the  following is assumed of a group `G` of one of its concrete implementations:

  * The function `gens(G)` returns the list of generators of `G`.
  * The function `one(G)` returns the identity element of `G`.

**Examples**

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3))
Group((1,2),(1,2,3))

julia> gens(G)
2-element Vector{Perm{Int16}}:
 (1,2)
 (1,2,3)

julia> ngens(G)
2

julia> minimal_words(G)
OrderedDict{Perm{Int16}, Vector{Int64}} with 6 entries:
  ()      => []
  (1,2)   => [1]
  (1,2,3) => [2]
  (1,3)   => [1, 2]
  (2,3)   => [2, 1]
  (1,3,2) => [2, 2]
```

There  is a constructor of a group with arbitrary type elements, `Group(l)` where  `l isa AbstractVector{T}` constructs a `Groupof{T}` which knows only the general methods in this module. The examples above use `Group(AbstractVector{<:Perm})`  which constructs  a `PermGroup`  which has more efficient methods.

for  further information on  the functions defined  in this module, look at the  docstrings of `Group,  gens, ngens, comm,  orbit, orbits, transversal, words_transversal,  centralizer,  stabilizer,  center,  normalizer,  some_words, minimal_words,   word,  in,   elements,  length,   order,  conjugacy_class, conjugacy_classes, classreps, nconjugacy_classes, fusion_conjugacy_classes, position_class,  isabelian,  iscyclic,  istrivial,  rand, transporting_elt, intersect, Hom, kernel, Coset`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L1-L46' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.Group' href='#PermGroups.Groups.Group'>#</a>
**`PermGroups.Groups.Group`** &mdash; *Type*.



`(G::Group)(i...)`

A Group used as a function takes integer arguments in `eachindex(gens(W))`. This  constructs  the  element  of  `G`  product of the generators with the specified  indices. An argument  can also be  negative, then the inverse of the corresponding generator is used.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3))
Group((1,2),(1,2,3))

julia> G(2,1,-2) # returns gens(G)[2]*gens(G)[1]/gens(G)[2]
(1,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L152-L167' class='documenter-source'>source</a><br>


`Group(l::AbstractVector{T}[,one]) where T`

A  group may be constructed  from a list of  `l` elements of the same type. These  elements must respond to  the functions `*` and  `inv`. If it is not possible  to compute  `one` from  `l` (because  `l[1]` does  not respond to `one`,  or  `l`  is  empty  and  `T`  does  not respond to `one`), then the identity element of the group must be given as a second argument.

```julia-repl
julia> G=Group([[-1 -1;1 0]])
Group([[-1 -1; 1 0]])

julia> elements(G)
3-element Vector{Matrix{Int64}}:
 [1 0; 0 1]
 [-1 -1; 1 0]
 [0 1; -1 -1]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L869-L888' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.generators' href='#PermGroups.Groups.generators'>#</a>
**`PermGroups.Groups.generators`** &mdash; *Function*.



`gens(G::Group)` or `generators(G::Group)` is the `Vector` of generators of `G`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L144' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.number_of_generators' href='#PermGroups.Groups.number_of_generators'>#</a>
**`PermGroups.Groups.number_of_generators`** &mdash; *Function*.



`ngens(G::Group)` or `number_of_generators(G::Group)` is the number of generators of `G`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L148' class='documenter-source'>source</a><br>

<a id='Base.one-Tuple{Group}' href='#Base.one-Tuple{Group}'>#</a>
**`Base.one`** &mdash; *Method*.



`one(G::Group)` returns the identity element of `G`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L139' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.orders_of_generators' href='#PermGroups.Groups.orders_of_generators'>#</a>
**`PermGroups.Groups.orders_of_generators`** &mdash; *Function*.



`orders_of_generators(G::Group)` or `ordergens`

The list of orders of the generators (this may be expensive to compute so could be worth being cached in `G`).


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L706-L711' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.ontuples' href='#PermGroups.Groups.ontuples'>#</a>
**`PermGroups.Groups.ontuples`** &mdash; *Function*.



`ontuples(t,g)`

Assume  that `t` is a  `Vector` or a `NTuple`.  `ontuples` is the action of `g` given by `(t,g)->map(x->x^g,t)`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L178-L183' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.onsets' href='#PermGroups.Groups.onsets'>#</a>
**`PermGroups.Groups.onsets`** &mdash; *Function*.



`onsets(s,g)`

Assume that `s` is a set, represented as a sorted list without repetitions. `onsets` is the action of `g` given by `(s,g)->sort!(map(x->x^g,s))`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L186-L191' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbit-Tuple{AbstractVector, Any}' href='#PermGroups.Perms.orbit-Tuple{AbstractVector, Any}'>#</a>
**`PermGroups.Perms.orbit`** &mdash; *Method*.



`orbit(gens::AbstractVector,p,action::Function=^)`

`orbit(G::Group,p,action::Function=^)`

the orbit of point `p` under repeated action of generators `gens`. The type of  point `p` should be hashable. The  default action of a group element is `^`.  For example if `g` is a permutation  and `p` an integer, `p^g` is the image  of `p` by `g`; if `h` and  `g` are group elements, then `h^g` is the conjugate  `inv(g)*h*g`. If  a group  is given  instead of  generators, the orbit under `gens(G)` is returned.

```julia-repl
julia> orbit([Perm(1,2),Perm(2,3)],1)
3-element Vector{Int64}:
 1
 2
 3

julia> orbit([Perm(1,2),Perm(2,3)],[1,3],ontuples)
6-element Vector{Vector{Int64}}:
 [1, 3]
 [2, 3]
 [1, 2]
 [3, 2]
 [2, 1]
 [3, 1]

julia> orbit([Perm(1,2),Perm(2,3)],[1,3],(v,g)->sort(v.^g)) # "OnSets"
3-element Vector{Vector{Int64}}:
 [1, 3]
 [2, 3]
 [1, 2]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L194-L228' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbits-Tuple{Group, Any}' href='#PermGroups.Perms.orbits-Tuple{Group, Any}'>#</a>
**`PermGroups.Perms.orbits`** &mdash; *Method*.



`orbits(gens::Vector,v,action=^;trivial=true)`

`orbits(G,v,action=^;trivial=true)`

the  orbits on `v`  of the repeated  action of `gens`;  the elements of `v` should  be hashable. If a  group is given instead  of generators, the orbit under  `gens(G)` is returned. If `trivial=false` the one-element orbits are not returned.

```julia-repl
julia> G=Group(Perm(1,2),Perm(2,3));
julia> orbits(G,1:4)
2-element Vector{Vector{Int64}}:
 [1, 2, 3]
 [4]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L336-L353' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.elements-Tuple{Group}' href='#PermGroups.Groups.elements-Tuple{Group}'>#</a>
**`PermGroups.Groups.elements`** &mdash; *Method*.



`elements(G::Group)` the list of elements of G


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L590' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.transversal' href='#PermGroups.Groups.transversal'>#</a>
**`PermGroups.Groups.transversal`** &mdash; *Function*.



`transversal(G::Group,p,action::Function=^)`

returns  an `OrderedDict` `t` with keys  `orbit(G,p,action)` and where `t[x]` is an element  of  `G`  such  that  `x==action(p,t[x])`.  Like  `orbit`,  it thus requires the type of `p` to be hashable.

```julia-repl
julia> G=Group(Perm(1,2),Perm(2,3));
julia> transversal(G,1)
OrderedDict{Int64, Perm{Int16}} with 3 entries:
  1 => ()
  2 => (1,2)
  3 => (1,3,2)
```

orbit functions can take any action of `G` as keyword argument

```julia-repl
julia> transversal(G,(1,2),ontuples)
OrderedDict{Tuple{Int64, Int64}, Perm{Int16}} with 6 entries:
  (1, 2) => ()
  (2, 1) => (1,2)
  (1, 3) => (2,3)
  (3, 1) => (1,3,2)
  (2, 3) => (1,2,3)
  (3, 2) => (1,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L244-L271' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.words_transversal' href='#PermGroups.Groups.words_transversal'>#</a>
**`PermGroups.Groups.words_transversal`** &mdash; *Function*.



`words_transversal(gens,p,action::Function=^)`

A   transversal   recording   words.   returns   a  `Dict`  `t`  with  keys `orbit(gens,p,action)` and where `t[x]` is a sequence of integers such that `x==action(p,prod(gens[t[x]]))`,  that is for each element `x` of the orbit of `p` describes as a word in `gens` an element bringing `p` to `x`.

```julia-repl
julia> words_transversal([Perm(1,2),Perm(2,3)],1)
OrderedDict{Int64, Vector{Int64}} with 3 entries:
  1 => []
  2 => [1]
  3 => [1, 2]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L297-L312' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.centralizer' href='#PermGroups.Groups.centralizer'>#</a>
**`PermGroups.Groups.centralizer`** &mdash; *Function*.



`centralizer(G::Group,p,action=^)`

computes  the subgroup of elements `g` of `G` such that `action(p,g)==p`.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3));
julia> centralizer(G,1)
Group((2,3))
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L356-L366' class='documenter-source'>source</a><br>


`centralizer(G::Group,H::Group)` the centralizer in `G` of the group `H`

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3))
Group((1,2),(1,2,3))

julia> centralizer(G,Group(Perm(1,2)))
Group((1,2))
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L369-L378' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.center' href='#PermGroups.Groups.center'>#</a>
**`PermGroups.Groups.center`** &mdash; *Function*.



`center(G::Group)` the center of `G`

```julia-repl
julia> G=Group(Perm(1,2),Perm(3,4),Perm(1,3)*Perm(2,4))
Group((1,2),(3,4),(1,3)(2,4))

julia> center(G)
Group((1,2)(3,4))
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L410-L420' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.stabilizer' href='#PermGroups.Groups.stabilizer'>#</a>
**`PermGroups.Groups.stabilizer`** &mdash; *Function*.



`stabilizer(G::Group,s,action=^)`

computes  the subgroup of elements `g` of `G` such that `action(p,g)==p`.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3,4))
Group((1,2),(1,2,3,4))
```

Assume that `s` is a set, represented as a sorted list without repetitions. `onsets` is the  action  of  `g∈  G`  given  by  `(g,p)->sort(p.^g)`.

```julia-repl
julia> stabilizer(G,[1,2],onsets)
Group((3,4),(1,2))
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L381-L396' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.normalizer' href='#PermGroups.Groups.normalizer'>#</a>
**`PermGroups.Groups.normalizer`** &mdash; *Function*.



`normalizer(G::Group,H::Group)` the normalizer of `H` in `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L134' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.word-Tuple{Group, Any}' href='#PermGroups.Groups.word-Tuple{Group, Any}'>#</a>
**`PermGroups.Groups.word`** &mdash; *Method*.



`word(G::Group,w)` a minimal word in `gens(G)` representing element `w` of `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L576' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.comm' href='#PermGroups.Groups.comm'>#</a>
**`PermGroups.Groups.comm`** &mdash; *Function*.



`comm(a,b)` or `commutator(a,b)` is `a^-1*b^-1*a*b`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L174' class='documenter-source'>source</a><br>

<a id='Base.length-Tuple{Group}' href='#Base.length-Tuple{Group}'>#</a>
**`Base.length`** &mdash; *Method*.



`length(G::Group)` the number of elements of `G`.

`length(T,G)` do the computation with the integer type `T`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L598-L602' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.classreps-Tuple{Group}' href='#PermGroups.Groups.classreps-Tuple{Group}'>#</a>
**`PermGroups.Groups.classreps`** &mdash; *Method*.



`class_representatives(G::Group)`  or `classreps`

representatives  of  conjugacy  classes  of  `G`.  By  default  queries the attribute  `G.classreps`, and if this attribute  is present it will be used by `conjugacy_classes`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L672-L678' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.conjugacy_classes' href='#PermGroups.Groups.conjugacy_classes'>#</a>
**`PermGroups.Groups.conjugacy_classes`** &mdash; *Function*.



`conjugacy_classes(G::Group)` conjugacy classes of `G` (as a `Vector{ConjugacyClass}`)


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L625-L628' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.conjugacy_class' href='#PermGroups.Groups.conjugacy_class'>#</a>
**`PermGroups.Groups.conjugacy_class`** &mdash; *Function*.



`conjugacy_class(G::Group,g)` the class of `g`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L652' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.number_of_conjugacy_classes' href='#PermGroups.Groups.number_of_conjugacy_classes'>#</a>
**`PermGroups.Groups.number_of_conjugacy_classes`** &mdash; *Function*.



`number_of_conjugacy_classes(G::Group)` or `nconjugacy_classes`

the number of conjugacy classes of `G`"


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L687-L691' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.position_class' href='#PermGroups.Groups.position_class'>#</a>
**`PermGroups.Groups.position_class`** &mdash; *Function*.



`position_class(G::Group,g)` index of conjugacy class to which `g` belongs


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L655' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.fusion_conjugacy_classes' href='#PermGroups.Groups.fusion_conjugacy_classes'>#</a>
**`PermGroups.Groups.fusion_conjugacy_classes`** &mdash; *Function*.



`fusion_conjugacy_classes(H::Group,G::Group)`

A `Vector{Int}` telling for each conjugacy class of subgroup `H` of which class of `G` is is a subset


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L662-L667' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.minimal_words' href='#PermGroups.Groups.minimal_words'>#</a>
**`PermGroups.Groups.minimal_words`** &mdash; *Function*.



`minimal_words(G::Group)`

returns  a `Dict` giving for each element of `G` a minimal positive word in the generators representing it.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3));
julia> minimal_words(G)
OrderedDict{Perm{Int16}, Vector{Int64}} with 6 entries:
  ()      => []
  (1,2)   => [1]
  (1,2,3) => [2]
  (1,3)   => [1, 2]
  (2,3)   => [2, 1]
  (1,3,2) => [2, 2]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L427-L444' class='documenter-source'>source</a><br>


`minimal_words(G::Group,w)`

Gives all expressions of `w` as words in the generators of `G` of minimal length (uses `minimal_words(G)`).

```julia-repl
julia> G=Group(Perm(1,2),Perm(2,3));

julia> minimal_words(G,Perm(1,3))
2-element Vector{Vector{Int64}}:
 [1, 2, 1]
 [2, 1, 2]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L451-L464' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.words-Tuple{Group}' href='#PermGroups.Groups.words-Tuple{Group}'>#</a>
**`PermGroups.Groups.words`** &mdash; *Method*.



`words(G::Group;minimal=false)`

returns  a  for  each  element  of  `G`  a  positive word in the generators representing  it. These words are not guaranteed minimal unless the keyword `minimal=true` is given (which makes the function somewhat slower).

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3));

julia> words(G)
6-element Vector{Vector{Int64}}:
 []
 [1]
 [2]
 [1, 2]
 [2, 1]
 [1, 2, 1]

julia> words(G;minimal=true)
6-element Vector{Vector{Int64}}:
 []
 [1]
 [2]
 [1, 2]
 [2, 1]
 [2, 2]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L521-L549' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.transporting_element' href='#PermGroups.Groups.transporting_element'>#</a>
**`PermGroups.Groups.transporting_element`** &mdash; *Function*.



`transporting_elt(G,p,q,action=^)`    or `transporting_element(G,p,q,action=^)`

returns  an  element  `g∈  G`  such  that  `p^g==q` (or `action(p,g)==q` if `action` is given), if such a `g` exists, and nothing otherwise. The set of possible `g` forms a right coset of the centralizer of p in G.

```julia-repl
julia> g=Group(perm"(1,2,3)(6,7)",perm"(3,4,5)(7,8)")
Group((1,2,3)(6,7),(3,4,5)(7,8))

julia> transporting_elt(g,1,5)
(1,5,4,3,2)

julia> transporting_elt(g,1,6)

julia> transporting_elt(g,[1,2,3,4],[2,3,4,5],(s,g)->sort(s.^g))
(1,2,3,4,5)(6,7,8)

julia> transporting_elt(g,[1,2,3,4],[3,4,5,2],(s,g)->s.^g)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L731-L753' class='documenter-source'>source</a><br>

<a id='Base.intersect-Tuple{Group, Group}' href='#Base.intersect-Tuple{Group, Group}'>#</a>
**`Base.intersect`** &mdash; *Method*.



`intersect(G::Group, H::Group)` the intersection as a group


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L802' class='documenter-source'>source</a><br>

<a id='Base.rand-Tuple{Group}' href='#Base.rand-Tuple{Group}'>#</a>
**`Base.rand`** &mdash; *Method*.



`rand(W::Group)` a random element of `W`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L724' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.isabelian' href='#PermGroups.Groups.isabelian'>#</a>
**`PermGroups.Groups.isabelian`** &mdash; *Function*.



`isabelian(G::Group)` whether `G` is abelian


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L715' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.iscyclic' href='#PermGroups.Groups.iscyclic'>#</a>
**`PermGroups.Groups.iscyclic`** &mdash; *Function*.



`iscyclic(G::Group)` whether `G` is cyclic


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L718' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.istrivial' href='#PermGroups.Groups.istrivial'>#</a>
**`PermGroups.Groups.istrivial`** &mdash; *Function*.



`istrivial(G::Group)` whether `G` is trivial


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L721' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.Hom' href='#PermGroups.Groups.Hom'>#</a>
**`PermGroups.Groups.Hom`** &mdash; *Type*.



`Hom(S::Group,T::Group,images)`

builds an object representing the homomorphism from `S` to `T` which maps `gens(S)` to `images`.

```julia-repl
julia> S=Group(Perm(1,2),Perm(2,3))
Group((1,2),(2,3))

julia> T=Group(Perm(1,2))
Group((1,2))

julia> h=Hom(S,T,[T(1),T(1)])
Hom(Group((1,2),(2,3))→ Group((1,2));[(1,2), (2,3)]↦ [(1,2), (1,2)]

julia> h(S(1,2)) # the image by h
()
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L814-L833' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.kernel' href='#PermGroups.Groups.kernel'>#</a>
**`PermGroups.Groups.kernel`** &mdash; *Function*.



`kernel(h::Hom)` the kernel of the homomorphism `h`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L848' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.Coset' href='#PermGroups.Groups.Coset'>#</a>
**`PermGroups.Groups.Coset`** &mdash; *Type*.



`Coset(G::Group,phi=one(G))`  constructs the (left)  coset `G.phi` where `G isa  Group{<:T}` and `phi isa  T`, as an object  of type `Cosetof{T}`. This general  coset knows only the general methods for a coset `C=G.phi` defined in this module, which are

  * `Group(C)` returns `G`.
  * `isone(C)` returns `true` iff `phi in G`
  * `one(C)` returns the trivial coset `G.1`
  * `length(C)` returns `length(G)`
  * `elements(C)` returns `elements(G).*Ref(phi)`
  * `x in C` returns `x/phi in G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L1010-L1022' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.NormalCoset' href='#PermGroups.Groups.NormalCoset'>#</a>
**`PermGroups.Groups.NormalCoset`** &mdash; *Type*.



`NormalCoset(G::Group,phi=one(G))`  constructs the coset `C=G.phi` where `G isa  Group{<:T}` and `phi isa T`,  as an object of type `NormalCosetof{T}`. It  is assumed that `phi` normalizes `G`. This general coset knows only the general  methods defined for normal cosets in the module `Groups`, which in addition to those defined for cosets (see `Coset`) are

  * `inv(C)` return `G.inv(phi)` (assumed equal to `inv(phi).G`)
  * `C*D` given another coset `G.psi` returns `G.phi*psi`
  * `C/D` given another coset `G.psi` returns `G.phi*inv(psi)`
  * `C^D` given another coset `G.psi` returns `G.inv(psi)*phi*psi`
  * `C^n` returns `G.phi^n`
  * `order(C)` the smallest `n` such that `isone(C^n)`

The  conjugacy  classes  of  a  normal  coset  `G.phi`  are relative to the conjugation action of `G` on `G.phi`. We have the functions `conjugacy_classes, nconjugacy_classes, classreps, position_class`.

Finally  the function  `G/H` for  two groups  constructs the  quotient as a group of `NormalCoset`s, and `fusion_conjugacy_classes(H::NormalCoset,G::NormalCoset)`   expresses   the fusion of conjugacy classes.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L1030-L1052' class='documenter-source'>source</a><br>


<a id='Permutation-groups'></a>

<a id='Permutation-groups-1'></a>

# Permutation groups

<a id='PermGroups' href='#PermGroups'>#</a>
**`PermGroups`** &mdash; *Module*.



This  module is a port  of some GAP functionality  on permutation groups. A `PermGroup` is a `Group` where `gens` are `Perm`s. It depends on the modules `Groups` and `Perms` which could be independent packages on their own.

```julia-repl
julia> G=Group([Perm(i,i+1) for i in 1:2])
Group((1,2),(2,3))

julia> collect(G) # PermGroups are iterators over their elements
6-element Vector{Perm{Int16}}:
 ()
 (1,2)
 (1,3,2)
 (2,3)
 (1,2,3)
 (1,3)

julia> last_moved(G)  # maximum moved point of any element of `G`
3

julia> orbits(G) # the orbits are orbits on points it moves
1-element Vector{Vector{Int16}}:
 [1, 2, 3]

julia> Perm(1,2) in G
true

julia> Perm(1,2,4) in G
false
```

`elements`,   `in`  and   other  functions   are  computed   on  `G`  using Schreier-Sims theory, that is computing the following

```julia-repl
julia> get_stabchain(G)
b:1 c:Group((1,2),(2,3))
  δ:1=>(),2=>(1,2),3=>(1,3,2)
b:2 c:Group((2,3))
  δ:2=>(),3=>(2,3)
```

See the docstring of `stabchain` for explanations.

There are efficient methods for `PermGroups` for the functions `in, length, elements,   position_class`.  The  function   `on_classes`  determines  the permutation  of the conjugacy classes effected by an automorphism. Finally, we   give  application  to  the  group   of  simultaneous  row  and  column permutations of a matrix: see `stab_onmats, Perm`.

finally, benchmarks on julia 1.9

```benchmark
julia> @btime collect(symmetric_group(8));
  1.921 ms (50128 allocations: 3.29 MiB)

julia> @btime some_words(symmetric_group(8));
  6.441 ms (80971 allocations: 10.88 MiB)

julia> @btime elements(symmetric_group(8)); # Gap takes 8 ms
  1.565 ms (49539 allocations: 3.71 MiB)

julia> rubik_gens=[
  perm"(1,3,8,6)(2,5,7,4)(9,33,25,17)(10,34,26,18)(11,35,27,19)",
  perm"(9,11,16,14)(10,13,15,12)(1,17,41,40)(4,20,44,37)(6,22,46,35)",
  perm"(17,19,24,22)(18,21,23,20)(6,25,43,16)(7,28,42,13)(8,30,41,11)",
  perm"(25,27,32,30)(26,29,31,28)(3,38,43,19)(5,36,45,21)(8,33,48,24)",
  perm"(33,35,40,38)(34,37,39,36)(3,9,46,32)(2,12,47,29)(1,14,48,27)",
  perm"(41,43,48,46)(42,45,47,44)(14,22,30,38)(15,23,31,39)(16,24,32,40)"];

julia> @btime length(Int128,Group(rubik_gens)) # Gap takes 5ms
  4.906 ms (104874 allocations: 13.64 MiB)
43252003274489856000
```

Note  the use of  `Int128` in `length`:  the computation does  not fit in an `Int64`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L1-L74' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.last_moved-Tuple{PermGroup}' href='#PermGroups.Perms.last_moved-Tuple{PermGroup}'>#</a>
**`PermGroups.Perms.last_moved`** &mdash; *Method*.



`last_moved(a::Perm)` is the largest integer moved by a


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L340' class='documenter-source'>source</a><br>


`last_moved(G::PermGroup)` the largest moved point by any `g∈ G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L101' class='documenter-source'>source</a><br>

<a id='Base.in-Tuple{Perm, PermGroup}' href='#Base.in-Tuple{Perm, PermGroup}'>#</a>
**`Base.in`** &mdash; *Method*.



`x in G` for `G` a group: whether `x` is an element of `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Groups.jl#L595' class='documenter-source'>source</a><br>

<a id='PermGroups.on_classes' href='#PermGroups.on_classes'>#</a>
**`PermGroups.on_classes`** &mdash; *Function*.



`on_classes(G, aut)`

`aut`  is an automorphism of  the group `G` (for  a permutation group, this could  be  given  as  a  permutation  normalizing  `G`).  The result is the permutation of `1:nconjugacy_classes(G)` induced by `aut`.

```julia-repl
julia> W=Group(Perm(1,2),Perm(2,3),Perm(4,5),Perm(5,6))
Group((1,2),(2,3),(4,5),(5,6))

julia> on_classes(W,Perm(1,4,2,5,3,6))
(2,4)(3,7)(6,8)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L612-L626' class='documenter-source'>source</a><br>

<a id='PermGroups.symmetric_group' href='#PermGroups.symmetric_group'>#</a>
**`PermGroups.symmetric_group`** &mdash; *Function*.



`symmetric_group(n::Int)`  The symmetric group of degree n


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L765' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.onmats' href='#PermGroups.Perms.onmats'>#</a>
**`PermGroups.Perms.onmats`** &mdash; *Function*.



`onmats(m::AbstractMatrix,g::Perm)` synonym for `invpermute(m,g;dims=(1,2))` or `invpermute(m,g,g)`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/Perms.jl#L474-L477' class='documenter-source'>source</a><br>

<a id='PermGroups.stab_onmats' href='#PermGroups.stab_onmats'>#</a>
**`PermGroups.stab_onmats`** &mdash; *Function*.



`stab_onmats([G,]M;extra=nothing)`

If  `onmats(m,p)=^(M,p;dims=(1,2))`, and  the argument  `G` is given (which should   be  a  `PermGroup`)   this  is  just   a  fast  implementation  of `centralizer(G,M,onmats)`.   If  `G`   is  omitted   it  is   taken  to  be `symmetric_group(size(M,1))`.  The  program  uses sophisticated algorithms, and  can handle matrices up to 80×80. If a list `extra` is given the result centralizes also `extra`.

```julia-repl
julia> stab_onmats((1:30)'.*(1:30).%15)
Group((1,16),(4,19),(11,26),(14,29),(2,17),(7,22),(8,23),(13,28),(6,21),(9,24),(1,4)(2,8)(3,12)(6,9)(7,13)(11,14)(16,19)(17,23)(18,27)(21,24)(22,28)(26,29),(3,18),(12,27),(1,11)(2,7)(4,14)(5,10)(8,13)(16,26)(17,22)(19,29)(20,25)(23,28),(5,20),(10,25),(15,30))
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L797-L811' class='documenter-source'>source</a><br>

<a id='PermGroups.Perm_onmats' href='#PermGroups.Perm_onmats'>#</a>
**`PermGroups.Perm_onmats`** &mdash; *Function*.



`Perm_onmats(M, N[, m ,n])`

returns `p` such that `onmats(N,p)=M` if it exists, `nothing` otherwise; so is just an efficient version of `transporting_elt(symmetric_group(size(M,1)),N,M,onmats)`  If  in  addition the vectors `m` and `n` are given, `p` should satisfy `invpermute(n,p)=m`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L831-L838' class='documenter-source'>source</a><br>

<a id='PermGroups.ProdIterator' href='#PermGroups.ProdIterator'>#</a>
**`PermGroups.ProdIterator`** &mdash; *Type*.



A  `ProdIterator([i₁,…,iₙ])`  takes  a  list  `i₁,…,iₙ`  of  iterators  and iterates  on all the products `i₁[j₁]*…*iₙ[jₙ]`  (where the inner loop `jₙ` runs  the  fastest).  It  tries  to  be  fast  by re-using partial products `i₁[j₁]*…*iₖ[jₖ]` for `k<n`.

It is used internally for iterating over a permutation group.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/600195568d6e3e242ab57a373d8ab048dcc2d812/src/PermGroups.jl#L636-L643' class='documenter-source'>source</a><br>

