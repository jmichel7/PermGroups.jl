"""
This package implements permutations and some functions of them. It depends
only  on  the  package  `Combinat`.

This  package  follows  the  design  of  permutations  in the GAP language.
`Perm`s  are permutations  of the  set `1:n`,  represented internally  as a
vector  of `n`  integers holding  the images  of `1:n`.  The integer `n` is
called  the degree  of the  permutation. In  this package,  as in  GAP (and
contrary  to the philosophy of Magma or the package `Permutations.jl`), two
permutations of different  degrees  can  be  multiplied (the result has the
larger  degree). Two permutations  are equal if  and only if  they move the
same points in the same way, so two permutations of different degree can be
equal; the degree is thus an implementation detail so usually it should not
be used. One should rather use the function `last_moved`.

This  design makes it  easy to multiply  permutations coming from different
groups, like a group and one of its subgroups. It has a negligible overhead
compared to the design where the degree is fixed.

The default constructor for a permutation uses the list of images of `1:n`,
like  `Perm([2,3,1,5,4,6])`.  Often  it  is  more  convenient  to use cycle
decompositions:    the   above   permutation    has   cycle   decomposition
`(1,2,3)(4,5)`    thus   can   be    written   `Perm(1,2,3)*Perm(4,5)`   or
`perm"(1,2,3)(4,5)"`  (this last form  can parse a  permutation coming from
GAP  or the default printing at the REPL).  The list of images of `1:n` can
be  recovered from the permutation by  the function `perm`; note that equal
permutations  with different degrees will  have different `perm`. Note that
the  default constructor  tests the  validity of  the input  by calling the
`julia` function `isperm`. To have a faster constructor which does not test
the input, use the keyword argument `check=false`.

The  complete type of a permutation  is `Perm{T}` where `T<:Integer`, where
`Vector{T}`  is the type of the vector which holds the image of `1:n`. This
can be used to save space or time. For instance `Perm{UInt8}(1,2,3)` can be
used for Weyl groups of rank≤8 since they permute at most 240 roots. If `T`
is  not specified we take it to be  `Int16` since this is a good compromise
between   speed,  compactness  and  possible  size  of  `n`.  One  can  mix
permutations of different integer types; they are promoted to the wider one
when multiplying.

# Examples of operations with permutations
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

julia> a\\b     # left quotient inv(a)*b
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

`Perm`s have methods `copy`, `hash`, `==`, so they can be keys in hashes or
elements  of sets; two permutations are equal  if they move the same points
to  the same images. They have methods `cmp`, `isless` (lexicographic order
on   moved  points)  so  they  can  be  sorted.  `Perm`s  are  scalars  for
broadcasting.

Other  methods on  permutations are  `cycles, cycletype, reflection_length,
mappingPerm, restricted, support, sortPerm, Perm_rowcol, preimage, randPerm`.

No  method is given in  this package to enumerate  `Perm`s; you can use the
method  `permutations` from `Combinat`,  iterate `Combinat.Permutations` or
iterate the elements of `symmetric_group` from `PermGroups`.
"""
module Perms

export restricted, orbit, orbits, order, Perm, last_moved, cycles,
  cycletype, support, @perm_str, first_moved, preimage, perm,
  reflength, reflection_length,
  mappingPerm, sortPerm, Perm_rowcol, randPerm, invpermute, onmats

using Combinat: tally, collectby, arrangements

const Idef=Int16 # the default type T for Perms

using AbstractPermutations

"""
`struct Perm{T<:Integer}`

A  Perm represents a permutation  of the set `1:n`  and is implemented by a
`struct`  with one field,  a `Vector{T}` holding  the images of `1:n`. When
showing  a `Perm` at the REPL, the cycle decomposition is displayed as well
as the type if it is not `$Idef`. The default constructor checks the input,
unless the keyword argument `check=false` is given.

```julia-repl
julia> Perms.Perm_(UInt8[1,3,2,4])
Perm{UInt8}: (2,3)
```
"""
struct Perm{T<:Integer}<:AbstractPermutations.AbstractPermutation
  d::Vector{T}
# inner constructor that bypasses all checks
  global Perm_(d::AbstractVector{T}) where T=new{T}(d)
end

Base.eltype(p::Perm{T}) where T=T

# next 3 methods for AbstractPermutations tests to pass
AbstractPermutations.degree(p::Perm)=last_moved(p)
AbstractPermutations.inttype(p::Perm)=eltype(p)
Perm{T}(v::AbstractVector{T};check) where T<:Integer=Perm(v;check)

"""
`perm(p::Perm)` returns the data field of a `Perm`.
```julia-repl
julia> perm(Perm(2,3;degree=4))
4-element Vector{Int16}:
 1
 3
 2
 4
```
"""
perm(p::Perm)=p.d

#---------------- Constructors ---------------------------------------
function Perm(v::AbstractVector{<:Integer};check=true)
  if check && !isperm(v) throw(ArgumentError("not a permutation")) end
  Perm_(v)
end

"""
`Perm{T}(x::Integer...)where T<:Integer`

returns  a cycle.  For example  `Perm{Int8}(1,2,3)` constructs the cycle
`(1,2,3)` as a `Perm{Int8}`. If omitted `{T}` is taken to be `{$Idef}`.
"""
function Perm{T}(x::Vararg{<:Integer,N};degree=0)where {T<:Integer,N}
  if isempty(x) return Perm_(T(1):T(degree)) end
  d=T.(1:max(degree,maximum(x)))
  for i in 1:length(x)-1
    d[x[i]]=x[i+1]
  end
  d[x[end]]=x[1]
  if length(x)>2 && !isperm(d) throw(ArgumentError("not a permutation")) end
  Perm_(d)
end

Perm(x::Integer...;degree=0)=Perm{Idef}(x...;degree)

Base.convert(::Type{Perm{T}},p::Perm{T1}) where {T,T1}=T==T1 ? p : Perm_(T.(p.d))

"""
   `Perm{T}(p::Perm) where T<:Integer`

   change the type of `p` to `Perm{T}`
   for example `Perm{Int8}(Perm(1,2,3))==Perm{Int8}(1,2,3)`
"""
Perm{T}(p::Perm) where {T<:Integer}=convert(Perm{T},p)

"""
  @perm"..."

makes a `Perm{$Idef}` from a string; allows GAP-style `perm"(1,2)(5,6,7)(4,9)"`.
If the cycle decomposition is preceded by `"Perm{T}:"` the constructed 
permutation is of type `T`.

```julia-repl
perm"Perm{UInt8}:(1,2)(3,4)"
Perm{UInt8}: (1,2)(3,4)
```
"""
macro perm_str(s::String)
  s=replace(s,"\\n"=>"\n")
  if (m=match(r"^Perm{(\w*)}:",s))!=nothing
    T=eval(Meta.parse(m[1]))
    s=s[m.match.ncodeunits+1:end]
  else T=Idef
  end
  res=Perm{T}()
  if match(r"^\s*\(\s*\)\s*$",s)!==nothing return res end
  while match(r"^\s*$"s,s)===nothing
    m=match(r"^\s*\((\s*\d+\s*,)+\s*\d+\)"s,s)
    if m===nothing throw(ArgumentError("malformed permutation: ",s)) end
    s=s[m.match.ncodeunits+1:end]
    res*=Perm{T}(Meta.parse(replace(m.match,r"\s*"=>"")).args...)
  end
  res::Perm
end

"""
just for fun: Perm[1 2;4 9;5 6 7]=perm"(1,2)(4,9)(5,6,7)"
"""
function Base.typed_hvcat(::Type{Perm},a::Tuple{Vararg{Int,N}},
  b::Vararg{Int,N1})where {N,N1}
  res=Perm()
  for i in a
    res*=Perm(Iterators.take(b,i)...)
    b=Iterators.drop(b,i)
  end
  res
end

"""
`Matrix(p::Perm,n=last_moved(p))`
the  permutation matrix  for `p`  operating on  `n` points.

```julia-repl
julia> Matrix(Perm(2,3,4),5)
5×5 Matrix{Bool}:
 1  0  0  0  0
 0  0  1  0  0
 0  0  0  1  0
 0  1  0  0  0
 0  0  0  0  1
```
"""
Base.Matrix(p::Perm,n=last_moved(p))=[j==i^p for i in 1:n, j in 1:n]

"""
`Perm{T}(m::AbstractMatrix)`
If  `m` is a  permutation matrix, returns  the corresponding permutation of
type `T`. If omitted, `T` is taken to be `$Idef`.

```julia-repl
julia> Perm([0 1 0;0 0 1;1 0 0])
(1,2,3)
```
"""
Perm(m::AbstractMatrix{<:Integer})=Perm{Idef}(m)

function Perm{T}(m::AbstractMatrix{<:Integer}) where T<:Integer
  l=map(x->findfirst(!iszero,x),eachrow(m))
  if size(m,1)!=size(m,2) || any(x->count(!iszero,x)!=1,eachrow(m)) || 
    !isperm(l) || !all(i->isone(m[i,l[i]]),axes(m,1))
    throw(ArgumentError("not a permutation matrix"))
  end
  Perm_(T.(l))
end

#---------------------------------------------------------------------
Base.one(p::Perm{T}) where T=Perm{T}(;degree=length(p.d))
Base.one(::Type{Perm{T}}) where T=Perm_(T[])
Base.isone(p::Perm)=@inbounds all(i->p.d[i]==i,eachindex(p.d))
Base.copy(p::Perm)=Perm_(copy(p.d))
Base.deepcopy(p::Perm)=copy(p)

# Perms are scalars for broadcasting
Base.broadcastable(p::Perm)=Ref(p)

Base.typeinfo_implicit(::Type{Perm{T}}) where T=T==Idef

function Base.promote_rule(a::Type{Perm{T1}},b::Type{Perm{T2}})where {T1,T2}
  Perm{promote_type(T1,T2)}
end

function extend!(a::Perm{T},n::Integer)where T
  if length(a.d)<n append!(a.d,T(length(a.d)+1):T(n)) end
end

"""
 `promote_degree(a::Perm, b::Perm)` promotes `a` and `b` to the same type,
 then extends `a` and `b` to the same degree
"""
function promote_degree(a::Perm,b::Perm)
  a,b=promote(a,b)
  extend!(a,length(b.d))
  extend!(b,length(a.d))
  (a,b)
end

# hash is needed for using Perms in Sets or as keys in Dicts
function Base.hash(a::Perm, h::UInt)
  for (i,v) in pairs(a.d)
    if v!=i h=hash(v,h) end
  end
  h
end

# permutations need to be totally ordered to use them in sorted lists
function Base.isless(a::Perm, b::Perm)
  a,b=promote_degree(a,b)
  isless(a.d,b.d)
end

function Base.:(==)(a::Perm, b::Perm)
  a,b=promote_degree(a,b)
  a.d==b.d
end

" `last_moved(a::Perm)` is the largest integer moved by a"
function last_moved(a::Perm{T})where T
  @inbounds p=findlast(x->a.d[x]!=x,eachindex(a.d))
  isnothing(p) ? T(0) : T(p)
end

" `first_moved(a::Perm)` is the smallest integer moved by a"
function first_moved(a::Perm{T})where T
  p=findfirst(x->a.d[x]!=x,eachindex(a.d))
  isnothing(p) ? T(0) : T(p)
end

" `support(a::Perm)` is the sorted list of all points moved by `a`"
support(a::Perm{T}) where T=(T(1):T(length(a.d)))[[x!=y for (x,y) in enumerate(a.d)]]
# faster than findall(x->a.d[x]!=x,eachindex(a.d))

" for convenience: `sortPerm(a)=Perm(sortperm(a))`"
sortPerm(::Type{T},a::AbstractVector;k...) where T=Perm_(T.(sortperm(a;k...)))
sortPerm(a::AbstractVector;k...)=sortPerm(Idef,a;k...)

"""
`randPerm([T,]n::Integer)` a random permutation of `1:n` of type `T`.
If omitted `T` is taken to be `$Idef`
"""
randPerm(::Type{T},n::Integer) where T =sortPerm(T,rand(1:n,n))
randPerm(n::Integer)=randPerm(Idef,n)

#------------------ arithmetic on permutations --------------------------

function Base.:*(a::Perm, b::Perm)
  a,b=promote_degree(a,b)
  r=similar(a.d)
@inbounds for (i,v) in pairs(a.d) r[i]=b.d[v] end
  Perm_(r)
end

# this is a*=b without allocation
function mul!(a::Perm, b::Perm)
  a,b=promote_degree(a,b)
@inbounds for (i,v) in pairs(a.d) a.d[i]=b.d[v] end
  a
end

function Base.inv(a::Perm)
  r=similar(a.d)
  @inbounds for (i,v) in pairs(a.d) r[v]=i end
  Perm_(r)
end

"`preimage(i::Integer,p::Perm)` the preimage of `i` by `p` (same as image of `i` by `inv(p)` but does not need computing the inverse)."
function preimage(i::T,p::Perm)where T<:Integer
  p=findfirst(==(i),p.d)
  isnothing(p) ? i : T(p)
end

# less allocations than inv(a)*b
function Base.:\(a::Perm, b::Perm)
  a,b=promote_degree(a,b)
  r=similar(a.d)
@inbounds for (i,v) in pairs(a.d) r[v]=b.d[i] end
  Perm_(r)
end

# less allocations than inv(b)*a*b
function Base.:^(a::Perm, b::Perm)
  a,b=promote_degree(a,b)
  r=similar(a.d)
@inbounds for (i,v) in pairs(a.d) r[b.d[i]]=b.d[v] end
  Perm_(r)
end

# I do not know how to do this one faster
Base.:/(a::Perm, b::Perm)=a*inv(b)

@inline Base.:^(n::T, a::Perm) where T<:Integer=
  n>length(a.d) ? n : @inbounds T(a.d[n])

Base.:^(a::Perm, n::Integer)=n>=0 ? Base.power_by_squaring(a,n) :
                                    Base.power_by_squaring(inv(a),-n)

"""
`invpermute(l::AbstractVector,p::Perm)`

returns  `l` invpermuted by  `p`, a vector  `r` of same  length as `l` such
that `r[i^p]==l[i]` for `i` in `eachindex(l)`. This function corresponds to
the  GAP function  `Permuted`, but  we changed  the name  to fit  the Julia
conventions since `invpermute(l,p)==invpermute!(copy(l),perm(p))`.

```julia-repl
julia> invpermute([5,4,6],Perm(1,2,3))
3-element Vector{Int64}:
 6
 5
 4
```
"""
function invpermute(l::AbstractVector,a::Perm)
  res=similar(l)
@inbounds for i in eachindex(l) res[i^a]=l[i] end
  res
end

function Base.:^(l::AbstractVector,a::Perm)
  error("**** using old form\n")
end

"""
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
"""
invpermute(m::AbstractMatrix,p1,p2)=m[invpermute(axes(m,1),p1),invpermute(axes(m,2),p2)]

"""
`onmats(m::AbstractMatrix,g::Perm)` synonym for `invpermute(m,g;dims=(1,2))`
or `invpermute(m,g,g)`.
"""
onmats(m::AbstractMatrix,g::Perm)=invpermute(m,g,g)

"""
`invpermute(m::AbstractMatrix,p::Perm;dims=1)`

invpermutes  by `p` the rows, columns or  both of the matrix `m` depending on
the value of `dims`.

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
"""
function invpermute(m::AbstractMatrix,a::Perm;dims=1)
  if dims==2 m[:,invpermute(axes(m,2),a)]
  elseif dims==1 m[invpermute(axes(m,1),a),:]
  elseif dims==(1,2) onmats(m,a)
  end
end

#---------------------- cycles -------------------------

"""
`orbit(p::Perm,i::Integer)` returns the orbit of `p` on `i`.
"""
function orbit(p::Perm,i::Integer)
  res=[i]
  j=i
  while true
    j^=p
    if j==i return res end
    push!(res,j)
  end
end

"""
`orbits(a::Perm,d::AbstractVector{<:Integer};trivial=true)`

returns  the orbits of `a` on domain `d`, which should be a union of orbits
of `a`. If `trivial=false`, does not return the orbits of length `1`.

# Example
```julia-repl
julia> orbits(Perm(1,2)*Perm(4,5),1:5)
3-element Vector{Vector{Int64}}:
 [1, 2]
 [3]
 [4, 5]
```
"""
function orbits(a::Perm,domain::AbstractVector{T};trivial=true)where T<:Integer
  orbs=Vector{T}[]
  if isempty(domain) return orbs end
  to_visit=falses(maximum(domain))
@inbounds  to_visit[domain].=true
  for i in eachindex(to_visit)
    if !to_visit[i] continue end
    if !trivial && i^a==i
      @inbounds to_visit[i]=false
      continue
    end
    cyc=orbit(a,T(i))
@inbounds to_visit[cyc].=false
    push!(orbs,cyc)
  end
  orbs
end

# 20 times faster than GAP Cycles for randPerm(1000)
"""
  `cycles(a::Perm)` returns the cycles of `a`
# Example
```julia-repl
julia> cycles(Perm(1,2)*Perm(4,5))
2-element Vector{Vector{$Idef}}:
 [1, 2]
 [4, 5]
```
"""
cycles(a::Perm{T}) where T=orbits(a,T(1):T(last_moved(a));trivial=false)

function Base.show(io::IO, a::Perm{T})where T
  if !isperm(a.d) error("malformed permutation") end
  hasdecor=get(io,:limit,false)||get(io,:TeX,false)
  if !hasdecor print(io,"perm\"") end
  if T!=Idef && get(io,:typeinfo,nothing) in (Any, nothing)
    print(io,typeof(a),": ")
  end
  cyc=cycles(a)
  if isempty(cyc) print(io,"()")
  else for c in cyc print(io,"(",join(c,","),")") end
  end
  if !hasdecor print(io,"\"") end
end

Base.show(io::IO, ::MIME"text/plain", a::Perm{T}) where T=show(io,a)
  
#--- CycleLengths is an iterator on the cycle lengths of a permutation
struct CycleLengths{T}
  a::Perm{T}
  to_visit::Vector{Bool}
  function CycleLengths(a::Perm{T},domain=1:last_moved(a))where T
    to_visit=fill(false,maximum(domain;init=0))
@inbounds to_visit[domain].=true
    new{T}(a,to_visit)
  end
end

Base.IteratorSize(x::CycleLengths)=Base.SizeUnknown()
Base.eltype(x::CycleLengths)=Int

@inline function Base.iterate(c::CycleLengths,k=1)
  for i in k:length(c.to_visit)
@inbounds if !c.to_visit[i] continue end
    l=1
    j=i
    while true
@inbounds  c.to_visit[j]=false
      j^=c.a
      if j==i return l,i end
      l+=1
    end
  end
end
  
"""
`cycletype(a::Perm,domain::AbstractVector{<:Integer};trivial=true)`

`domain`  should be  a union  of orbits  of `a`.  Returns the  partition of
`length(domain)`  associated to the conjugacy class of `a` in the symmetric
group  of `domain`, with ones removed if `trivial=false` (in which case the
partition does not depend on `domain` but just on `support(a)`)

`cycletype(a::Perm)`

returns `cycletype(a,1:last_moved(a);trivial=false)`

# Example
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
"""
cycletype(a::Perm)=cycletype(a,1:last_moved(a);trivial=false)

function cycletype(a::Perm,domain;trivial=true)
  lengths=Int[]
  for l in CycleLengths(a,domain)
    if l>1 || trivial push!(lengths,l) end
  end
  sort!(lengths,rev=true)
end

function order(a::Perm)
  ord=1
  for l in CycleLengths(a)
    if l!=1 ord=lcm(ord,l) end
  end
  ord
end

"""
`reflection_length(p::Perm)` or `reflength`

gives  the  "reflection  length"  of  `p`  (when the symmetric group on `n`
points to which `p` belongs is interpreted as a reflection group on a space
of  dimension `n`), that is, the  minimum number of transpositions of which
`p` is the product.
"""
reflection_length(a::Perm)=sum(i->i-1,CycleLengths(a);init=0)
  
const reflength=reflection_length

" `sign(p::Perm)` is the signature of  the permutation `p`"
Base.sign(a::Perm)=(-1)^reflength(a)

"""
`restricted(p::Perm,domain::AbstractVector{<:Integer})`

`domain` should be a union of orbits of `p`; returns `p` restricted to `domain`

```julia-repl
julia> restricted(Perm(1,2)*Perm(3,4),3:4)
(3,4)
```
"""
function restricted(a::Perm{T},l::AbstractVector{<:Integer})where T
  v=collect(T(1):T(maximum(l)))
  for i in l v[i]=i^a end
  Perm_(v)
end

"""
`mappingPerm([::Type{T},]a::AbstractVector{<:Integer})`

given  a list  of positive  integers without  repetition `a`, this function
finds  a  `Perm{T}`  `p`  such  that  `sort(a).^p==a`.  This can be used to
translate  between arrangements and `Perm`s. If  omitted `T` is taken to be
`$Idef`.

```julia-repl
julia> p=mappingPerm([6,7,5])
(5,6,7)

julia> (5:7).^p
3-element Vector{Int64}:
 6
 7
 5
```
"""
mappingPerm(a)=mappingPerm(Idef,a)

function mappingPerm(::Type{T},a::AbstractVector{<:Integer})where T
  r=collect(1:maximum(a))
  r[sort(a)]=a
  Perm_(T.(r))
end

"""
`mappingPerm([::Type{T},]a,b)`

given  two lists of positive integers  without repetition `a` and `b`, this
function finds a `Perm{T}` `p` such that `a.^p==b`. If omitted `T` is taken
to be `$Idef`.

```julia-repl
julia> mappingPerm([1,2,5,3],[2,3,4,6])
(1,2,3,6,5,4)
```
"""
function mappingPerm(::Type{T},a,b)where T
  r=1:max(maximum(a),maximum(b))
  a=vcat(a,setdiff(r,a))
  b=vcat(b,setdiff(r,b))
  Perm_(T.(a))\Perm_(T.(b))
end
mappingPerm(a,b)=mappingPerm(Idef,a,b)

#------------------- constructor from 2 objects -------------------------
"""
  `Perm{T}(l::AbstractVector,l1::AbstractVector)`

returns  `p`, a  `Perm{T}`, such  that `invpermute(l1,p)==l`  if such a `p`
exists;  returns `nothing`  otherwise. If  not given  `{T}` is  taken to be
`{$Idef}`. Needs the `eltype` of `l` and `l1` to be sortable.

```julia-repl
julia> Perm([0,2,4],[4,0,2])
(1,3,2)
```
"""
function Perm{T}(l::AbstractVector,l1::AbstractVector)where T<:Integer
  p=sortperm(l)
  p1=sortperm(l1)
  @inbounds if view(l,p)==view(l1,p1) Perm_(T.(p1))\Perm_(T.(p)) end
end

Perm(l::AbstractVector,l1::AbstractVector)=Perm{Idef}(l,l1)

"""
`Perm_rowcol(m1::AbstractMatrix, m2::AbstractMatrix)`

whether `m1` can be obtained from `m2` by row/col permutations.

`m1` and `m2` should be rectangular matrices of the same size. The function
returns a `Tuple` of permutations `(p1,p2)` such that
`invpermute(m2,p1,p2)==m1` if such permutations exist, `nothing` otherwise.
The `eltype` of `m1` and `m2` must be sortable.

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
"""
function Perm_rowcol(m1::AbstractMatrix, m2::AbstractMatrix;debug=false)
  if size(m1)!=size(m2) throw(ArgumentError("not same dimensions")) end
  if isempty(m1) return [Perm(), Perm()] end
  dist(m,n)=count(i->m[i]!=n[i],eachindex(m))
  dist(m,n,dim,l)=dim==1 ? dist(m[l,:],n[l,:]) : dist(m[:,l],n[:,l])
  mm=[m1,m2]
  if debug print("# ", dist(m1, m2), "") end
  rcperm=[Perm(), Perm()],[Perm(), Perm()]
  crg=Vector{Int}[],Vector{Int}[]
  crg1=[axes(m1,1)],[axes(m1,2)]
  while true
    crg=crg1
    crg1=Vector{Int}[],Vector{Int}[]
    for dim in 1:2
      for g in crg[dim]
        invars=map(1:2) do i
          invar=map(j->map(k->tally(dim==1 ? mm[i][j,k] : mm[i][k,j]),
                           crg[3-dim]), g)
          p=mappingPerm(vcat(collectby(invar,g)...), g)
          rcperm[dim][i]*=p
          mm[i]=invpermute(mm[i],p,dims=dim)
          sort!(invar)
        end
        if invars[1]!=invars[2] return nothing end
        append!(crg1[dim], collectby(invars[1],g))
      end
    end
    if debug print("==>",dist(mm[1],mm[2])) end
    if crg==crg1 break end
  end
  function best(l,dim)
    if length(l)==1 return false end
    d=dist(mm[1], mm[2], dim, l)
#   if debug print("l=",l,"\n") end
    for e in mappingPerm.(arrangements(l,length(l)))
      m=dist(invpermute(mm[1], e;dims=dim), mm[2], dim, l)
      if m<d
        if debug print("\n",("rows","cols")[dim],l,":$d->",m) end
        rcperm[dim][1]*=e
        mm[1]=invpermute(mm[1],e;dims=dim)
        return true
      end
    end
    return false
  end
  while true
    s=false
    for dim in 1:2 for g in crg[dim] s=s || best(g,dim) end end
    if !s break end
  end
  if debug print("\n") end
  if !iszero(dist(mm...)) error("Perm_rowcol failed") end
  (rcperm[1][2]/rcperm[1][1],rcperm[2][2]/rcperm[2][1])
end

end
