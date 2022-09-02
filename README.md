
<a id='Permutations'></a>

<a id='Permutations-1'></a>

# Permutations

- [`PermGroups.Perms.Perm`](index.md#PermGroups.Perms.Perm-Tuple{Vararg{Integer}})
- [`PermGroups.Perms.Perm`](index.md#PermGroups.Perms.Perm-Tuple{AbstractVector, AbstractVector})
- [`PermGroups.Perms.Perm`](index.md#PermGroups.Perms.Perm)
- [`PermGroups.Perms.Perm`](index.md#PermGroups.Perms.Perm-Tuple{AbstractMatrix{<:Integer}})
- [`PermGroups.Perms.Perm`](index.md#PermGroups.Perms.Perm-Tuple{AbstractMatrix, AbstractMatrix})
- [`PermGroups.Perms.Perm_rowcol`](index.md#PermGroups.Perms.Perm_rowcol)
- [`PermGroups.Perms.cycles`](index.md#PermGroups.Perms.cycles-Tuple{Perm})
- [`PermGroups.Perms.cycletype`](index.md#PermGroups.Perms.cycletype-Tuple{Perm})
- [`PermGroups.Perms.largest_moved_point`](index.md#PermGroups.Perms.largest_moved_point-Tuple{PermGroup})
- [`PermGroups.Perms.largest_moved_point`](index.md#PermGroups.Perms.largest_moved_point-Tuple{Perm})
- [`PermGroups.Perms.mappingPerm`](index.md#PermGroups.Perms.mappingPerm)
- [`PermGroups.Perms.orbit`](index.md#PermGroups.Perms.orbit-Tuple{Perm, Integer})
- [`PermGroups.Perms.orbit`](index.md#PermGroups.Perms.orbit-Tuple{AbstractVector, Any})
- [`PermGroups.Perms.orbits`](index.md#PermGroups.Perms.orbits-Tuple{Perm})
- [`PermGroups.Perms.orbits`](index.md#PermGroups.Perms.orbits-Tuple{Group, Any})
- [`PermGroups.Perms.order`](index.md#PermGroups.Perms.order)
- [`PermGroups.Perms.randPerm`](index.md#PermGroups.Perms.randPerm)
- [`PermGroups.Perms.reflength`](index.md#PermGroups.Perms.reflength-Tuple{Perm})
- [`PermGroups.Perms.restricted`](index.md#PermGroups.Perms.restricted-Tuple{Perm, AbstractVector{<:Integer}})
- [`PermGroups.Perms.smallest_moved_point`](index.md#PermGroups.Perms.smallest_moved_point)
- [`PermGroups.Perms.sortPerm`](index.md#PermGroups.Perms.sortPerm)
- [`PermGroups.Perms.support`](index.md#PermGroups.Perms.support)
- [`PermGroups.Perms.@perm_str`](index.md#PermGroups.Perms.@perm_str)

<a id='PermGroups.Perms' href='#PermGroups.Perms'>#</a>
**`PermGroups.Perms`** &mdash; *Module*.



This package implements permutations and some functions of them. It depends only  on  the  package  `Combinat` (which itself depends on `Primes`). 

This  package  follows  the  design  of  permutations  in the GAP language. `Perm`s  are permutations  of the  set `1:n`,  represented internally  as a vector  of `n`  integers holding  the images  of `1:n`.  The integer `n` is called  the degree  of the  permutation. In  this package,  as in  GAP (and contrary  to the philosophy of Magma or the package `Permutations.jl`), two permutations of different  degrees  can  be  multiplied (the result has the larger  degree). Two permutations  are equal if  and only if  they move the same points in the same way, so two permutations of different degree can be equal; the degree is thus an implementation detail so usually it should not be used. One should rather use the function `largest_moved_point`.

This  design makes it  easy to multiply  permutations coming from different groups, like a group and one of its subgroups. It has a negligible overhead compared to the design where the degree is fixed.

The default constructor for a permutation uses the list of images of `1:n`, like  `Perm([2,3,1,5,4])`.  Often  it  is  more  convenient  to  use  cycle decompositions:    the   above   permutation    has   cycle   decomposition `(1,2,3)(4,5)`    thus   can   be    written   `Perm(1,2,3)*Perm(4,5)`   or `perm"(1,2,3)(4,5)"`  (this last form  can parse a  permutation coming from GAP  or the default printing at the REPL).  The list of images of `1:n` can be  recovered from the  permutation by the  function `vec`; note that equal permutations with different degrees will have different `vec`.

The  complete type of a permutation  is `Perm{T}` where `T<:Integer`, where `Vector{T}`  is the type of the vector which holds the image of `1:n`. This can  be used to save space or  time. For instance `Perm{UInt8}` can be used for  Weyl groups of rank≤8 since they permute  at most 240 roots. If `T` is not  specified we  take it  to be  `Int16` since  this is a good compromise between   speed,  compactness  and  possible  size  of  `n`.  One  can  mix permutations of different integer types; they are promoted to the wider one when multiplying.

**Examples of operations with permutations**

```julia-repl
julia> a=Perm(1,2,3)
(1,2,3)

julia> vec(a)
3-element Vector{Int16}:
 2
 3
 1

julia> a==Perm(vec(a))
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

julia> largest_moved_point(a)
3

julia> smallest_moved_point(a)
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

Other   methods   on   permutations   are  `cycles,  cycletype,  reflength, mappingPerm, sortPerm, Perm_rowcol`.

No  method is given in  this package to enumerate  `Perm`s; you can use the method   `arrangements`  from   `Combinat`  or   iterate  the  elements  of `symmetric_group` with `PermGroups`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L1-L118' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm' href='#PermGroups.Perms.Perm'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Type*.



`struct Perm{T<:Integer}`

A  Perm represents a permutation  of the set `1:n`  and is implemented by a `struct` with one field, a `Vector{T}` holding the images of `1:n`.

```julia-repl
julia> p=Perm(Int16[1,3,2,4])
(2,3)

julia> vec(p)
4-element Vector{Int16}:
 1
 3
 2
 4
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L127-L144' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Tuple{Vararg{Integer}}' href='#PermGroups.Perms.Perm-Tuple{Vararg{Integer}}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(x::Integer...)where T<:Integer`

returns  a cycle.  For example  `Perm{Int8}(1,2,3)` constructs the cycle    `(1,2,3)` as a `Perm{Int8}`. If omitted `{T}` is taken to be `{Int16}`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L155-L160' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Tuple{AbstractMatrix{<:Integer}}' href='#PermGroups.Perms.Perm-Tuple{AbstractMatrix{<:Integer}}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(m::AbstractMatrix)` If  `m` is a  permutation matrix, returns  the corresponding permutation of type `T`. If omitted, `T` is taken to be `Int16`.

```julia-repl
julia> m=[0 1 0;0 0 1;1 0 0]
3×3 Matrix{Int64}:
 0  1  0
 0  0  1
 1  0  0

julia> Perm(m)
(1,2,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L231-L246' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Tuple{AbstractVector, AbstractVector}' href='#PermGroups.Perms.Perm-Tuple{AbstractVector, AbstractVector}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(l::AbstractVector,l1::AbstractVector)`

returns `p`, a `Perm{T}`, such that `l1^p==l` if such a `p` exists; returns `nothing` otherwise. If not given `{T}` is taken to be `{Int16}`. Needs the elements of `l` and `l1` to be sortable.

```julia-repl
julia> Perm([0,2,4],[4,0,2])
(1,3,2)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L259-L270' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm-Tuple{AbstractMatrix, AbstractMatrix}' href='#PermGroups.Perms.Perm-Tuple{AbstractMatrix, AbstractMatrix}'>#</a>
**`PermGroups.Perms.Perm`** &mdash; *Method*.



`Perm{T}(m::AbstractMatrix,m1::AbstractMatrix;dims=1)`

returns  `p`, a `Perm{T}`, which  permutes the rows of  `m1` (the coluns of `m1`  if `dims=2`)  to bring  them to  those of  `m`, if such a `p` exists; returns  `nothing` otherwise. If not given  `{T}` is taken to be `{Int16}`. Needs the elements of `m` and `m1` to be sortable.

```julia-repl
julia> Perm([0 1 0;0 0 1;1 0 0],[1 0 0;0 1 0;0 0 1];dims=1)
(1,3,2)

julia> Perm([0 1 0;0 0 1;1 0 0],[1 0 0;0 1 0;0 0 1];dims=2)
(1,2,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L279-L294' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.@perm_str' href='#PermGroups.Perms.@perm_str'>#</a>
**`PermGroups.Perms.@perm_str`** &mdash; *Macro*.



@perm"..."

make a `Perm` from a string; allows GAP-style `perm"(1,2)(5,6,7)(4,9)"`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L183-L187' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.largest_moved_point-Tuple{Perm}' href='#PermGroups.Perms.largest_moved_point-Tuple{Perm}'>#</a>
**`PermGroups.Perms.largest_moved_point`** &mdash; *Method*.



`largest_moved_point(a::Perm)` is the largest integer moved by a


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L351' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.smallest_moved_point' href='#PermGroups.Perms.smallest_moved_point'>#</a>
**`PermGroups.Perms.smallest_moved_point`** &mdash; *Function*.



`smallest_moved_point(a::Perm)` is the smallest integer moved by a


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L354' class='documenter-source'>source</a><br>

<a id='Base.:^-Tuple{AbstractVector, Perm}' href='#Base.:^-Tuple{AbstractVector, Perm}'>#</a>
**`Base.:^`** &mdash; *Method*.



`Base.:^(l::AbstractVector,p::Perm)` 

returns `l` permuted by `p`, a vector `r` such that `r[i^p]==l[i]`

**Examples**

```julia-repl
julia> [5,4,6,1,7,5]^Perm(1,3,5,6,4)
6-element Vector{Int64}:
 1
 4
 5
 5
 6
 7
```

note that we follow here the convention for the GAP function `Permuted`, but this has the consequence that `sort(a)==a^inv(Perm(sortperm(a)))`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L419-L438' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.sortPerm' href='#PermGroups.Perms.sortPerm'>#</a>
**`PermGroups.Perms.sortPerm`** &mdash; *Function*.



for convenience: `sortPerm(a)=Perm(sortperm(a))`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L361' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.randPerm' href='#PermGroups.Perms.randPerm'>#</a>
**`PermGroups.Perms.randPerm`** &mdash; *Function*.



`randPerm([T,]n::Integer)` a random permutation of `1:n` of type `T`. If omitted `T` is taken to be `Int16`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L365-L368' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbit-Tuple{Perm, Integer}' href='#PermGroups.Perms.orbit-Tuple{Perm, Integer}'>#</a>
**`PermGroups.Perms.orbit`** &mdash; *Method*.



orbit(a::Perm,i::Integer) returns the orbit of a on i


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L511-L513' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbits-Tuple{Perm}' href='#PermGroups.Perms.orbits-Tuple{Perm}'>#</a>
**`PermGroups.Perms.orbits`** &mdash; *Method*.



`orbits(a::Perm,d::Vector=1:length(a.d))` 

returns the orbits of `a` on domain `d`

**Example**

```julia-repl
julia> orbits(Perm(1,2)*Perm(4,5),1:5)
3-element Vector{Vector{Int16}}:
 [1, 2]
 [3]
 [4, 5]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L525-L538' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.order' href='#PermGroups.Perms.order'>#</a>
**`PermGroups.Perms.order`** &mdash; *Function*.



`order(G::Group)` the number of elements of G


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L491' class='documenter-source'>source</a><br>


`order(a)` the smallest integer `i≥1` such that `isone(a^i)`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L550' class='documenter-source'>source</a><br>


`order(a::Perm)` is the order of the permutation a


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L631-L633' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.cycles-Tuple{Perm}' href='#PermGroups.Perms.cycles-Tuple{Perm}'>#</a>
**`PermGroups.Perms.cycles`** &mdash; *Method*.



`cycles(a::Perm)` returns the non-trivial cycles of `a`

**Example**

```julia-repl
julia> cycles(Perm(1,2)*Perm(4,5))
2-element Vector{Vector{Int16}}:
 [1, 2]
 [4, 5]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L554-L563' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.cycletype-Tuple{Perm}' href='#PermGroups.Perms.cycletype-Tuple{Perm}'>#</a>
**`PermGroups.Perms.cycletype`** &mdash; *Method*.



`cycletype(a::Perm;domain=1:length(a.d),trivial=false)`

returns  the  partition  of  `maximum(domain)`  associated to the conjugacy class of `a` in the symmetric group of `domain`, with ones removed (thus it does  not  depend  on  `domain`  but  just  on  the  moved  points)  unless `trivial=true`.

**Example**

```julia-repl
julia> cycletype(Perm(1,2)*Perm(4,5))
2-element Vector{Int64}:
 2
 2

julia> cycletype(Perm(1,2)*Perm(4,5);trivial=true)
3-element Vector{Int64}:
 2
 2
 1

julia> cycletype(Perm(1,2)*Perm(4,5);trivial=true,domain=1:6)
4-element Vector{Int64}:
 2
 2
 1
 1
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L582-L610' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.support' href='#PermGroups.Perms.support'>#</a>
**`PermGroups.Perms.support`** &mdash; *Function*.



`support(a::Perm)` is the set of all points moved by `a`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L357' class='documenter-source'>source</a><br>

<a id='Base.sign' href='#Base.sign'>#</a>
**`Base.sign`** &mdash; *Function*.



`sign(a::Perm)` is the signature of  the permutation `a`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L658' class='documenter-source'>source</a><br>

<a id='Base.Matrix-Tuple{Perm, Any}' href='#Base.Matrix-Tuple{Perm, Any}'>#</a>
**`Base.Matrix`** &mdash; *Method*.



`Matrix(a::Perm,n=length(a.d))`  the  permutation matrix  for `a`  operating on  `n` points (by default, the degree of `a`). If given, `n` should be larger than `largest_moved_point(a)`.

```julia-repl
julia> Matrix(Perm(2,3,4),5)
5×5 Matrix{Bool}:
 1  0  0  0  0
 0  0  1  0  0
 0  0  0  1  0
 0  1  0  0  0
 0  0  0  0  1
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L214-L228' class='documenter-source'>source</a><br>

<a id='Base.:^-Tuple{AbstractMatrix, Perm}' href='#Base.:^-Tuple{AbstractMatrix, Perm}'>#</a>
**`Base.:^`** &mdash; *Method*.



`Base.:^(m::AbstractMatrix,p::Perm;dims=1)`

Applies the permutation `p` on the lines, columns or both of the matrix `m` depending on the value of `dims`

```julia-repl
julia> m=[3*i+j for i in 0:2,j in 1:3]
3×3 Matrix{Int64}:
 1  2  3
 4  5  6
 7  8  9

julia> p=Perm(1,2,3)
(1,2,3)

julia> m^p
3×3 Matrix{Int64}:
 7  8  9
 1  2  3
 4  5  6

julia> ^(m,p;dims=2)
3×3 Matrix{Int64}:
 3  1  2
 6  4  5
 9  7  8

julia> ^(m,p;dims=(1,2))
3×3 Matrix{Int64}:
 9  7  8
 3  1  2
 6  4  5
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L445-L479' class='documenter-source'>source</a><br>

<a id='Base.:^-Tuple{AbstractMatrix, Tuple{Perm, Perm}}' href='#Base.:^-Tuple{AbstractMatrix, Tuple{Perm, Perm}}'>#</a>
**`Base.:^`** &mdash; *Method*.



`Base.:^(m::AbstractMatrix,p::Tuple{Perm,Perm})`

given  a tuple `(p1,p2)` of  `Perm`s, applies `p1` to  the lines of `m` and `p2` to the columns of `m`.

```julia-repl
julia> m=[1 2 3;4 5 6;7 8 9]
3×3 Matrix{Int64}:
 1  2  3
 4  5  6
 7  8  9

julia> m^(Perm(1,2),Perm(2,3))
3×3 Matrix{Int64}:
 4  6  5
 1  3  2
 7  9  8
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L487-L506' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.restricted-Tuple{Perm, AbstractVector{<:Integer}}' href='#PermGroups.Perms.restricted-Tuple{Perm, AbstractVector{<:Integer}}'>#</a>
**`PermGroups.Perms.restricted`** &mdash; *Method*.



`restricted(a::Perm,l::AbstractVector{<:Integer})`

`l` should be a union of cycles of `p`; returns `p` restricted to `l`

```julia-repl
julia> restricted(Perm(1,2)*Perm(3,4),3:4)
(3,4)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L661-L670' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.reflength-Tuple{Perm}' href='#PermGroups.Perms.reflength-Tuple{Perm}'>#</a>
**`PermGroups.Perms.reflength`** &mdash; *Method*.



`reflength(a::Perm)`

is   the  "reflection   length"  of   `a`,  that   is,  minimum  number  of transpositions of which `a` is the product


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L636-L641' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.mappingPerm' href='#PermGroups.Perms.mappingPerm'>#</a>
**`PermGroups.Perms.mappingPerm`** &mdash; *Function*.



`mappingPerm(a)`

given  a list  of positive  integers without  repetition `a`, this function finds  a permutation  `p` such  that `sort(a).^p==a`.  This can  be used to translate between arrangements and `Perm`s.

```julia-repl
julia> p=mappingPerm([6,7,5])
(5,6,7)

julia> (5:7).^p
3-element Vector{Int16}:
 6
 7
 5
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L676-L693' class='documenter-source'>source</a><br>


`mappingPerm(a,b)`

given two lists of positive integers without repetition `a` and `b`, this function finds a permutation `p` such that `a.^p==b`.

```julia-repl
julia> mappingPerm([1,2,5,3],[2,3,4,6])
(1,2,3,6,5,4)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L701-L711' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.Perm_rowcol' href='#PermGroups.Perms.Perm_rowcol'>#</a>
**`PermGroups.Perms.Perm_rowcol`** &mdash; *Function*.



`Perm_rowcol(m1::AbstractMatrix, m2::AbstractMatrix)`

whether `m1` is conjugate to `m2` by row/col permutations.

`m1` and `m2` should be rectangular matrices of the same size. The function returns a pair of permutations `(p1,p2)` such that `m1^(p1,p2)==m2` if such permutations exist, `nothing` otherwise.

The entries of `m1` and `m2` must be sortable.

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
((1,2,4,5,3), (3,5,4))

julia> a^(p1,p2)==b
true
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Perms.jl#L720-L754' class='documenter-source'>source</a><br>


<a id='Groups'></a>

<a id='Groups-1'></a>

# Groups

- [`PermGroups.Groups.Coset`](index.md#PermGroups.Groups.Coset)
- [`PermGroups.Groups.Group`](index.md#PermGroups.Groups.Group)
- [`PermGroups.Groups.Hom`](index.md#PermGroups.Groups.Hom)
- [`PermGroups.Groups.NormalCoset`](index.md#PermGroups.Groups.NormalCoset)
- [`PermGroups.Groups.centralizer`](index.md#PermGroups.Groups.centralizer-Tuple{Group, Group})
- [`PermGroups.Groups.centralizer`](index.md#PermGroups.Groups.centralizer-Tuple{Group, Any})
- [`PermGroups.Groups.centre`](index.md#PermGroups.Groups.centre)
- [`PermGroups.Groups.classreps`](index.md#PermGroups.Groups.classreps-Tuple{Group})
- [`PermGroups.Groups.comm`](index.md#PermGroups.Groups.comm)
- [`PermGroups.Groups.conjugacy_class`](index.md#PermGroups.Groups.conjugacy_class)
- [`PermGroups.Groups.conjugacy_classes`](index.md#PermGroups.Groups.conjugacy_classes)
- [`PermGroups.Groups.elements`](index.md#PermGroups.Groups.elements-Tuple{Group})
- [`PermGroups.Groups.fusion_conjugacy_classes`](index.md#PermGroups.Groups.fusion_conjugacy_classes)
- [`PermGroups.Groups.gens`](index.md#PermGroups.Groups.gens)
- [`PermGroups.Groups.isabelian`](index.md#PermGroups.Groups.isabelian)
- [`PermGroups.Groups.iscyclic`](index.md#PermGroups.Groups.iscyclic)
- [`PermGroups.Groups.istrivial`](index.md#PermGroups.Groups.istrivial)
- [`PermGroups.Groups.kernel`](index.md#PermGroups.Groups.kernel)
- [`PermGroups.Groups.minimal_words`](index.md#PermGroups.Groups.minimal_words)
- [`PermGroups.Groups.nconjugacy_classes`](index.md#PermGroups.Groups.nconjugacy_classes)
- [`PermGroups.Groups.ngens`](index.md#PermGroups.Groups.ngens)
- [`PermGroups.Groups.normalizer`](index.md#PermGroups.Groups.normalizer)
- [`PermGroups.Groups.position_class`](index.md#PermGroups.Groups.position_class)
- [`PermGroups.Groups.stabilizer`](index.md#PermGroups.Groups.stabilizer)
- [`PermGroups.Groups.transporting_elt`](index.md#PermGroups.Groups.transporting_elt)
- [`PermGroups.Groups.transversal`](index.md#PermGroups.Groups.transversal)
- [`PermGroups.Groups.word`](index.md#PermGroups.Groups.word-Tuple{Group, Any})
- [`PermGroups.Groups.words`](index.md#PermGroups.Groups.words-Tuple{Group})
- [`PermGroups.Groups.words_transversal`](index.md#PermGroups.Groups.words_transversal)

<a id='PermGroups.Groups' href='#PermGroups.Groups'>#</a>
**`PermGroups.Groups`** &mdash; *Module*.



This module gives some basic functionality on groups.

`Group`  is  an  abstract  type,  but  the  following is assumed of a group `G` of one of its concrete implementations:

  * The function `gens(G)` returns the list of generators of `G`.
  * The function `one(G)` returns the identity element of `G`.

**Examples**

```julia-repl
julia> G=Group([Perm(1,2),Perm(1,2,3)])
Group([(1,2), (1,2,3)])

julia> gens(G)
2-element Vector{Perm{Int16}}:
 (1,2)  
 (1,2,3)

julia> ngens(G)
2

julia> minimal_words(G)
Dict{Perm{Int16}, Vector{Int64}} with 6 entries:
  ()      => []
  (1,2)   => [1]
  (1,3)   => [1, 2]
  (1,2,3) => [2]
  (2,3)   => [2, 1]
  (1,3,2) => [2, 2]
```

There  is a constructor of a group with arbitrary type elements, `Group(l)` where  `l isa AbstractVector{T}` constructs a `Groupof{T}` which knows only the general methods in this module. The examples above use `Group(AbstractVector{<:Perm})`  which constructs  a `PermGroup`  which has more and more efficient methods.

for  further information on  the functions defined  in this module, look at the  docstrings of `Group,  gens, ngens, comm,  orbit, orbits, transversal, words_transversal,  centralizer,  stabilizer,  centre,  normalizer,  words, minimal_words,   word,  in,   elements,  length,   order,  conjugacy_class, conjugacy_classes, classreps, nconjugacy_classes, fusion_conjugacy_classes, position_class,  isabelian,  iscyclic,  istrivial,  rand, transporting_elt, intersect, Hom, kernel, Coset`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L1-L46' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.Group' href='#PermGroups.Groups.Group'>#</a>
**`PermGroups.Groups.Group`** &mdash; *Type*.



`(G::Group)(i...)`

A Group used as a function takes integer arguments in `eachindex(gens(W))`. This  constructs  the  element  of  `G`  product of the generators with the specified  indices. An argument  can also be  negative, then the inverse of the corresponding generator is used.

```julia-repl
julia> G=Group([Perm(1,2),Perm(1,2,3)])
Group([(1,2), (1,2,3)])

julia> G(2,1,-2) # returns gens(G)[2]*gens(G)[1]/gens(G)[2]
(1,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L134-L149' class='documenter-source'>source</a><br>


`Group(l::AbstractVector{T}[,one]) where T`

A  group may be constructed  from a list of  `l` elements of the same type. These  elements must respond to  the functions `*` and  `inv`. If it is not possible  to compute  `one` from  `l` (because  `l[1]` does  not respond to `one`,  or  `l`  is  empty  and  `T`  does  not respond to `one`), then the identity element of the group must be given as a second argument.

```julia-repl
julia> G=Group([[-1 -1;1 0]])
Group([[-1 -1; 1 0]])

julia> elements(G)
3-element Vector{Matrix{Int64}}:
 [0 1; -1 -1]
 [1 0; 0 1]
 [-1 -1; 1 0]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L712-L731' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.gens' href='#PermGroups.Groups.gens'>#</a>
**`PermGroups.Groups.gens`** &mdash; *Function*.



`gens(G::Group)` returns the `Vector` of generators of `G`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L128' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.ngens' href='#PermGroups.Groups.ngens'>#</a>
**`PermGroups.Groups.ngens`** &mdash; *Function*.



`ngens(G::Group)` returns the number of generators of `G`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L131' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbit-Tuple{AbstractVector, Any}' href='#PermGroups.Perms.orbit-Tuple{AbstractVector, Any}'>#</a>
**`PermGroups.Perms.orbit`** &mdash; *Method*.



`orbit(gens::AbstractVector,p;action::Function=^)`

`orbit(G::Group,p;action::Function=^)`

the orbit of point `p` under repeated action of generators `gens`. The type of  point `p` should be hashable. The  default action of a group element is `^`.  For example if `g` is a permutation  and `p` an integer, `p^g` is the image  of `p` by `g`; if `h` and  `g` are group elements, then `h^g` is the conjugate  `inv(g)*h*g`. If  a group  is given  instead of  generators, the orbit under `gens(G)` is returned.

```julia-repl
julia> orbit([Perm(1,2),Perm(2,3)],1) 
3-element Vector{Int64}:
 1
 2
 3

julia> orbit([Perm(1,2),Perm(2,3)],[1,3];action=(v,g)->v.^g) # Gap "OnTuples"
6-element Vector{Vector{Int64}}:
 [1, 3]
 [2, 3]
 [1, 2]
 [3, 2]
 [2, 1]
 [3, 1]

julia> orbit([Perm(1,2),Perm(2,3)],[1,3];action=(v,g)->sort(v.^g)) # "OnSets"
3-element Vector{Vector{Int64}}:
 [1, 3]
 [2, 3]
 [1, 2]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L160-L194' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.orbits-Tuple{Group, Any}' href='#PermGroups.Perms.orbits-Tuple{Group, Any}'>#</a>
**`PermGroups.Perms.orbits`** &mdash; *Method*.



`orbits(gens::Vector,v;action=^,trivial=true)`

`orbits(G,v;action=^,trivial=true)`

the  orbits on `v`  of the repeated  action of `gens`;  the elements of `v` should  be hashable. If a  group is given instead  of generators, the orbit under  `gens(G)` is returned. If `trivial=false` the one-element orbits are not returned.

```julia-repl
julia> G=Group([Perm(1,2),Perm(2,3)]);
julia> orbits(G,1:4)
2-element Vector{Vector{Int64}}:
 [1, 2, 3]
 [4]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L290-L307' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.elements-Tuple{Group}' href='#PermGroups.Groups.elements-Tuple{Group}'>#</a>
**`PermGroups.Groups.elements`** &mdash; *Method*.



`elements(G::Group)` the list of elements of G


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L480' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.transversal' href='#PermGroups.Groups.transversal'>#</a>
**`PermGroups.Groups.transversal`** &mdash; *Function*.



`transversal(G::Group,p;action::Function=^)`

returns  a `Dict` `t` with keys  `orbit(G,p;action)` and where `t[x]` is an element  of  `G`  such  that  `x==action(p,t[x])`.  Like  `orbit`,  it thus requires the type of `p` to be hashable.

```julia-repl
julia> G=Group([Perm(1,2),Perm(2,3)]);
julia> transversal(G,1)
Dict{Int64, Perm{Int16}} with 3 entries:
  2 => (1,2)
  3 => (1,3,2)
  1 => ()
```

orbit functions can take any action of `G` as keyword argument

```julia-repl
julia> transversal(G,(1,2),action=(x,y)->x.^y)
Dict{Tuple{Int64, Int64}, Perm{Int16}} with 6 entries:
  (3, 2) => (1,3)
  (1, 2) => ()
  (3, 1) => (1,3,2)
  (1, 3) => (2,3)
  (2, 1) => (1,2)
  (2, 3) => (1,2,3)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L210-L237' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.words_transversal' href='#PermGroups.Groups.words_transversal'>#</a>
**`PermGroups.Groups.words_transversal`** &mdash; *Function*.



`words_transversal(gens,p;action::Function=^)`

A   transversal   recording   words.   returns   a  `Dict`  `t`  with  keys `orbit(G,p;action)`  and where  `t[x]` is  a `w`  is a sequence of integers such that `x==action(p,G(w...))`

```julia-repl
julia> words_transversal([Perm(1,2),Perm(2,3)],1)
Dict{Int64, Vector{Int64}} with 3 entries:
  2 => [1]
  3 => [1, 2]
  1 => []
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L251-L265' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.centralizer-Tuple{Group, Any}' href='#PermGroups.Groups.centralizer-Tuple{Group, Any}'>#</a>
**`PermGroups.Groups.centralizer`** &mdash; *Method*.



`centralizer(G::Group,p;action=^)`

computes  the subgroup of elements `g` of `G` such that `action(p,g)==p`.

```julia-repl
julia> G=Group([Perm(1,2),Perm(1,2,3)]);
julia> centralizer(G,1)
Group([(2,3)])
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L310-L320' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.centralizer-Tuple{Group, Group}' href='#PermGroups.Groups.centralizer-Tuple{Group, Group}'>#</a>
**`PermGroups.Groups.centralizer`** &mdash; *Method*.



`centralizer(G::Group,H::Group)` the centralizer in `G` of the group `H`

```julia-repl
julia> G=Group([Perm(1,2),Perm(1,2,3)])
Group([(1,2), (1,2,3)])

julia> centralizer(G,Group([Perm(1,2)]))
Group([(1,2)])
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L328-L337' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.centre' href='#PermGroups.Groups.centre'>#</a>
**`PermGroups.Groups.centre`** &mdash; *Function*.



`center(G::Group)` the centre of `G`

```julia-repl
julia> G=Group([Perm(1,2),Perm(3,4),Perm(1,3)*Perm(2,4)])
Group([(1,2), (3,4), (1,3)(2,4)])

julia> centre(G)
Group([(1,2)(3,4)])
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L360-L370' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.stabilizer' href='#PermGroups.Groups.stabilizer'>#</a>
**`PermGroups.Groups.stabilizer`** &mdash; *Function*.



`stabilizer(G::Group,s)`

Assume that `s` is a set, represented as a sorted list without repetitions. The  action  of  `g∈  G`  on  sets  is  given  by  `(g,p)->sort(p.^g)`. The *stabilizer* of `s` in `G` is the centralizer of `s` for that action.

```julia-repl
julia> G=Group([Perm(1,2),Perm(1,2,3,4)])
Group([(1,2), (1,2,3,4)])

julia> centralizer(G,[1,2];action=(s,g)->sort(s.^g))
Group([(3,4), (1,2), (1,2)(3,4)])

julia> stabilizer(G,[1,2])
Group([(3,4), (1,2), (1,2)(3,4)])
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L340-L357' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.normalizer' href='#PermGroups.Groups.normalizer'>#</a>
**`PermGroups.Groups.normalizer`** &mdash; *Function*.



`normalizer(G::Group,H::Group)` the normalizer of `H` in `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L120' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.word-Tuple{Group, Any}' href='#PermGroups.Groups.word-Tuple{Group, Any}'>#</a>
**`PermGroups.Groups.word`** &mdash; *Method*.



`word(G::Group,w)` a minimal word in `gens(G)` representing element `w` of `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L466' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.comm' href='#PermGroups.Groups.comm'>#</a>
**`PermGroups.Groups.comm`** &mdash; *Function*.



`comm(a,b)` or `commutator(a,b)` is `a^-1*b^-1*a*b`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L156' class='documenter-source'>source</a><br>

<a id='Base.length-Tuple{Group}' href='#Base.length-Tuple{Group}'>#</a>
**`Base.length`** &mdash; *Method*.



`length(G::Group)` the number of elements of G


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L488' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.classreps-Tuple{Group}' href='#PermGroups.Groups.classreps-Tuple{Group}'>#</a>
**`PermGroups.Groups.classreps`** &mdash; *Method*.



`classreps(G::Group)` 

representatives of conjugacy classes of `G`. Fills `G.classreps`. If this field is filled it is used by  `conjugacy_classes`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L533-L538' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.conjugacy_classes' href='#PermGroups.Groups.conjugacy_classes'>#</a>
**`PermGroups.Groups.conjugacy_classes`** &mdash; *Function*.



`conjugacy_classes(G::Group)` conjugacy classes of `G` (as a `Vector{Vector}`)


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L494' class='documenter-source'>source</a><br>


`conjugacy_classes(G::Group,i::Integer)` the `i`-th class of `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L506' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.conjugacy_class' href='#PermGroups.Groups.conjugacy_class'>#</a>
**`PermGroups.Groups.conjugacy_class`** &mdash; *Function*.



`conjugacy_class(G::Group,g)` the class of `g`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L515' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.nconjugacy_classes' href='#PermGroups.Groups.nconjugacy_classes'>#</a>
**`PermGroups.Groups.nconjugacy_classes`** &mdash; *Function*.



`nconjugacy_classes(G::Group)` the number of conjugacy classes of `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L547' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.position_class' href='#PermGroups.Groups.position_class'>#</a>
**`PermGroups.Groups.position_class`** &mdash; *Function*.



`position_class(G::Group,g)` index of conjugacy class to which `g` belongs


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L518' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.fusion_conjugacy_classes' href='#PermGroups.Groups.fusion_conjugacy_classes'>#</a>
**`PermGroups.Groups.fusion_conjugacy_classes`** &mdash; *Function*.



`fusion_conjugacy_classes(H::Group,G::Group)`

A `Vector{Int}` telling for each conjugacy class of subgroup `H` of which class of `G` is is a subset


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L523-L528' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.minimal_words' href='#PermGroups.Groups.minimal_words'>#</a>
**`PermGroups.Groups.minimal_words`** &mdash; *Function*.



`minimal_words(G::Group)`

returns  a `Dict` giving for each element of `G` a minimal positive word in the generators representing it.

```julia-repl
julia> G=Group([Perm(1,2),Perm(1,2,3)]);
julia> minimal_words(G)
Dict{Perm{Int16}, Vector{Int64}} with 6 entries:
  ()      => []
  (1,2)   => [1]
  (1,3)   => [1, 2]
  (1,2,3) => [2]
  (2,3)   => [2, 1]
  (1,3,2) => [2, 2]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L377-L394' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.words-Tuple{Group}' href='#PermGroups.Groups.words-Tuple{Group}'>#</a>
**`PermGroups.Groups.words`** &mdash; *Method*.



`words(G::Group)`

returns  a `Dict`  giving for  each element  of `G`  a positive word in the generators representing it. It is faster than `minimal_words` but the words are not guaranteed minimal.

```julia-repl
julia> G=Group([Perm(1,2),Perm(1,2,3)]);
julia> words(G)
Dict{Perm{Int16}, Vector{Int64}} with 6 entries:
  ()      => []
  (1,2)   => [1]
  (1,3)   => [1, 2]
  (1,2,3) => [2]
  (2,3)   => [2, 1]
  (1,3,2) => [1, 2, 1]
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L401-L419' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.transporting_elt' href='#PermGroups.Groups.transporting_elt'>#</a>
**`PermGroups.Groups.transporting_elt`** &mdash; *Function*.



`transporting_elt(G,p,q;action=^)`   

returns  an  element  `g∈  G`  such  that  `p^g==q` (or `action(p,g)==q` if `action` is given), if such a `g` exists, and nothing otherwise. The set of possible `g` forms a right coset of the centralizer of p in G.

```julia-repl
julia> g=Group(perm"(1,2,3)(6,7)",perm"(3,4,5)(7,8)")
Group([(1,2,3)(6,7), (3,4,5)(7,8)])

julia> transporting_elt(g,1,5)
(1,5,4,3,2)

julia> transporting_elt(g,1,6)

julia> transporting_elt(g,[1,2,3,4],[2,3,4,5];action=(s,g)->sort(s.^g))
(1,2,3,4,5)(6,7,8)

julia> transporting_elt(g,[1,2,3,4],[3,4,5,2];action=(s,g)->s.^g)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L579-L600' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.isabelian' href='#PermGroups.Groups.isabelian'>#</a>
**`PermGroups.Groups.isabelian`** &mdash; *Function*.



`isabelian(G::Group)` whether `G` is abelian


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L563' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.iscyclic' href='#PermGroups.Groups.iscyclic'>#</a>
**`PermGroups.Groups.iscyclic`** &mdash; *Function*.



`iscyclic(G::Group)` whether `G` is cyclic


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L566' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.istrivial' href='#PermGroups.Groups.istrivial'>#</a>
**`PermGroups.Groups.istrivial`** &mdash; *Function*.



`istrivial(G::Group)` whether `G` is trivial


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L569' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.Hom' href='#PermGroups.Groups.Hom'>#</a>
**`PermGroups.Groups.Hom`** &mdash; *Type*.



`Hom(S::Group,T::Group,images)`

builds an object representing the homomorphism from `S` to `T` which maps `gens(S)` to `images`.

```julia-repl
julia> S=Group([Perm(1,2),Perm(2,3)])
Group([(1,2), (2,3)])

julia> T=Group([Perm(1,2)])
Group([(1,2)])

julia> h=Hom(S,T,[T(1),T(1)])
Hom(Group([(1,2), (2,3)])→ Group([(1,2)]);[(1,2), (2,3)]↦ [(1,2), (1,2)]

julia> h(S(1,2)) # the image by h
()
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L657-L676' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.kernel' href='#PermGroups.Groups.kernel'>#</a>
**`PermGroups.Groups.kernel`** &mdash; *Function*.



`kernel(h::Hom)` the kernel of the homomorphism `h`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L691' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.Coset' href='#PermGroups.Groups.Coset'>#</a>
**`PermGroups.Groups.Coset`** &mdash; *Type*.



`Coset(phi,G)`  constructs the coset  `G.phi` where `G  isa Group{<:T}` and `phi  isa T`, as an  object of type `Cosetof{T}`.  This general coset knows only  the general  methods for  a coset  `C=G.phi` defined  in this module, which are

  * `Group(C)` if `C==G.phi` returns `G`.
  * `isone(C)` returns `true` iff `phi in G`
  * `one(C)` returns the trivial coset `G.1`
  * `length(C)` returns `length(G)`
  * `elements(C)` returns `elements(G).*Ref(phi)`
  * `x in C` returns `x/phi in G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L837-L849' class='documenter-source'>source</a><br>

<a id='PermGroups.Groups.NormalCoset' href='#PermGroups.Groups.NormalCoset'>#</a>
**`PermGroups.Groups.NormalCoset`** &mdash; *Type*.



`NormalCoset(phi,G)`  constructs the coset  `C=G.phi` where `G  isa Group{<:T}` and `phi  isa T`, as an  object of type `NormalCosetof{T}`.  It is assumed that `phi` normalizes `G`. This general coset knows oly the general methods defined for normal cosets in this module, which in addition to those defined for cosets (see `Coset`) are

  * `inv(C)` return `G.inv(phi)` (assumed euqal to `inv(phi).G`)
  * `C*D` given another coset `G.psi` returns `G.phi*psi`
  * `C/D` given another coset `G.psi` returns `G.phi*inv(psi)`
  * `C^D` given another coset `G.psi` returns `G.inv(psi)*phi*psi`
  * `C^n` returns `G.phi^n`
  * `order(C)` the smallest `n` such that `isone(C^n)`

The  conjugacy  classes  of  a  normal  coset  `G.phi`  are relative to the conjugation action of `G` on `G.phi`. We have the functions `conjugacy_classes, nconjugacy_classes, classreps, position_class`.

Finally  the function  `G/H` for  two groups  constructs the  quotient as a group of `NormalCoset`s, and `fusion_conjugacy_classes(H::NormalCoset,G::NormalCoset)`   expresses   the fusion of conjugacy classes.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L857-L878' class='documenter-source'>source</a><br>


<a id='Permutation-groups'></a>

<a id='Permutation-groups-1'></a>

# Permutation groups

- [`PermGroups.Groups`](index.md#PermGroups.Groups)
- [`PermGroups.Perms`](index.md#PermGroups.Perms)
- [`PermGroups.Perm_onmats`](index.md#PermGroups.Perm_onmats)
- [`PermGroups.base`](index.md#PermGroups.base)
- [`PermGroups.centralizers`](index.md#PermGroups.centralizers)
- [`PermGroups.on_classes`](index.md#PermGroups.on_classes)
- [`PermGroups.onmats`](index.md#PermGroups.onmats)
- [`PermGroups.stab_onmats`](index.md#PermGroups.stab_onmats)
- [`PermGroups.symmetric_group`](index.md#PermGroups.symmetric_group)
- [`PermGroups.transversals`](index.md#PermGroups.transversals)

<a id='PermGroups' href='#PermGroups'>#</a>
**`PermGroups`** &mdash; *Module*.



This  module is a port  of some GAP functionality  on permutation groups. A `PermGroup` is a `Group` where `gens` are `Perm`s. It depends on the modules `Groups` and `Perms` which could be independent packages on their own.

**Examples**

```julia-repl
julia> G=Group([Perm(i,i+1) for i in 1:2])
Group([(1,2), (2,3)])

# PermGroups are iterators over their elements
julia> collect(G)  
6-element Vector{Perm{Int16}}:
 (1,2)
 (1,3,2)
 ()
 (1,2,3)
 (1,3)
 (2,3)

julia> largest_moved_point(G)  # maximum moved point of any element of `G`
3

julia> orbits(G) # the orbits are orbits on points it moves
1-element Vector{Vector{Int64}}:
 [1, 2, 3]

julia> Perm(1,2) in G
true

julia> Perm(1,2,4) in G
false

# `elements`,  `in` test and  other functions are  computed on `G` using
# Schreier-Sims theory, that is computing the following

julia> base(G) # a list of points that no element of G fixes
2-element Vector{Int16}:
 1
 2

julia> centralizers(G) # the i-th element is C_G(base[1:i-1])
2-element Vector{PermGroup{Int16}}:
 Group([(1,2), (2,3)])
 Group([(2,3)])

# i-th element is the transversal of centralizer[i] on base[i]
julia> transversals(G)
2-element Vector{Dict{Int16, Perm{Int16}}}:
 Dict(2 => (1,2), 3 => (1,3,2), 1 => ())
 Dict(2 => (), 3 => (2,3))
```

The  code refers  to the  Handbook of  computational group theory, by Holt, Eick,  O'Brien, chapter 4  for basic algorithms  on permutation groups. See the docstrings for `base, transversals, centralizers` for more details.

There are efficient methods for `PermGroups` for the functions `in, length, elements,   position_class`.  The  function   `on_classes`  determines  the permutation  of the conjugacy classes effected by an automorphism. Finally, we   give  application  to  the  group   of  simultaneous  row  and  column permutations of a matrix: see `onmats, stab_onmats, Perm_onmats`.

finally, benchmarks on julia 1.8

```benchmark
julia> @btime collect(symmetric_group(8));
  2.673 ms (129965 allocations: 5.76 MiB)

julia> @btime words(symmetric_group(8));
  7.155 ms (86019 allocations: 11.00 MiB)
  
julia> @btime elements(symmetric_group(8));
  1.565 ms (49539 allocations: 3.71 MiB)
```

Gap `Elements(SymmetricGroup(8))` takes 8 ms

```benchmark
julia> rubik_gens=[
  perm"(1,3,8,6)(2,5,7,4)(9,33,25,17)(10,34,26,18)(11,35,27,19)",
  perm"(9,11,16,14)(10,13,15,12)(1,17,41,40)(4,20,44,37)(6,22,46,35)",
  perm"(17,19,24,22)(18,21,23,20)(6,25,43,16)(7,28,42,13)(8,30,41,11)",
  perm"(25,27,32,30)(26,29,31,28)(3,38,43,19)(5,36,45,21)(8,33,48,24)",
  perm"(33,35,40,38)(34,37,39,36)(3,9,46,32)(2,12,47,29)(1,14,48,27)",
  perm"(41,43,48,46)(42,45,47,44)(14,22,30,38)(15,23,31,39)(16,24,32,40)];

julia> @btime length(Group(rubik_gens);type=Int128)
  4.906 ms (104874 allocations: 13.64 MiB)
43252003274489856000
```

Note  the use of  `type=` in `length`:  the computation does  not fit in an `Int64`. GAP takes about the same time to compute the size of the group.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L1-L92' class='documenter-source'>source</a><br>

<a id='PermGroups.Perms.largest_moved_point-Tuple{PermGroup}' href='#PermGroups.Perms.largest_moved_point-Tuple{PermGroup}'>#</a>
**`PermGroups.Perms.largest_moved_point`** &mdash; *Method*.



`largest_moved_point(G::PermGroup)` the largest moved point by any `g∈ G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L110' class='documenter-source'>source</a><br>

<a id='PermGroups.base' href='#PermGroups.base'>#</a>
**`PermGroups.base`** &mdash; *Function*.



`base(G::PermGroup)` A `Vector` of points stabilized by no element of `G` 


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L236' class='documenter-source'>source</a><br>

<a id='PermGroups.centralizers' href='#PermGroups.centralizers'>#</a>
**`PermGroups.centralizers`** &mdash; *Function*.



`centralizers(G::PermGroup)`  

for  `i in  eachindex(base(G))` the  `i`-th element  is the  centralizer of `base(G)[1:i-1]`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L216-L221' class='documenter-source'>source</a><br>

<a id='PermGroups.transversals' href='#PermGroups.transversals'>#</a>
**`PermGroups.transversals`** &mdash; *Function*.



`transversals(G::PermGroup)`

returns a list whose `i`-th element is the transversal of `G.centralizers[i]` on `G.base[i]`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L226-L231' class='documenter-source'>source</a><br>

<a id='Base.in-Tuple{Perm, PermGroup}' href='#Base.in-Tuple{Perm, PermGroup}'>#</a>
**`Base.in`** &mdash; *Method*.



`x in G` for `G` a group: whether `x` is an element of `G`


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/Groups.jl#L485' class='documenter-source'>source</a><br>

<a id='PermGroups.on_classes' href='#PermGroups.on_classes'>#</a>
**`PermGroups.on_classes`** &mdash; *Function*.



`on_classes(G, aut)`

`aut`  is an automorphism of  the group `G` (for  a permutation group, this could  be  given  as  a  permutation  normalizing  `G`).  The result is the permutation of `1:nconjugacy_classes(G)` induced ny `aut`.

```julia-repl
julia> WF=rootdatum("3D4")
³D₄

julia> on_classes(Group(WF),WF.phi)
Perm{Int64}: (2,8,7)(5,13,12)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L288-L302' class='documenter-source'>source</a><br>

<a id='PermGroups.symmetric_group' href='#PermGroups.symmetric_group'>#</a>
**`PermGroups.symmetric_group`** &mdash; *Function*.



`symmetric_group(n::Int)`  The symmetric group of degree n


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L412' class='documenter-source'>source</a><br>

<a id='PermGroups.onmats' href='#PermGroups.onmats'>#</a>
**`PermGroups.onmats`** &mdash; *Function*.



`onmats(m::AbstractMatrix,g::Perm)` simultaneous action of `g` on   the columns and rows of `m`.


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L416-L419' class='documenter-source'>source</a><br>

<a id='PermGroups.stab_onmats' href='#PermGroups.stab_onmats'>#</a>
**`PermGroups.stab_onmats`** &mdash; *Function*.



`stab_onmats([G,]M;extra=nothing)`

If  `onmats(m,p)=^(M,p;dims=(1,2))`, and  the argument  `G` is given (which should   be  a  `PermGroup`)   this  is  just   a  fast  implementation  of `centralizer(G,M;action=onmats)`.  If  `G`  is  omitted  it  is taken to be `symmetric_group(size(M,1))`.  The  program  uses sophisticated algorithms, and can handle matrices up to 80×80. If a list `extra` is given the result centralizes also `extra`.

```julia-repl
julia> uc=UnipotentCharacters(complex_reflection_group(34));

julia> stab_onmats(fourier(uc.families[20]))
Group([(7,38), (39,44)(40,43)(41,42)])
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L450-L466' class='documenter-source'>source</a><br>

<a id='PermGroups.Perm_onmats' href='#PermGroups.Perm_onmats'>#</a>
**`PermGroups.Perm_onmats`** &mdash; *Function*.



`Perm_onmats(M, N[, m ,n])` 

If  `onmats(M,p)=^(M,p;dims=(1,2))`, return `p`  such that `onmats(M,p)=N`; so is just an efficient version of `transporting_elt(symmetric_group(size(M,1)),M,N;action=onmats)`    If   in addition the vectors `m` and `n` are given, `p` should satisfy `m^p=n`.

```julia-repl
julia> m=cartan(:D,12);

julia> n=^(m,Perm(1,5,2,8,12,4,7)*Perm(3,9,11,6);dims=(1,2));

julia> Perm_onmats(m,n)
(1,5,2,8,12,4,7)(3,9,11,6)
```


<a target='_blank' href='https://github.com/jmichel7/PermGroups.jl/blob/49f9afe96bdb493445230a38f96a80a14e3efa31/src/PermGroups.jl#L486-L502' class='documenter-source'>source</a><br>

