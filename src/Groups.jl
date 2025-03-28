"""
This module gives some basic functionality on groups.

`Group`  is  an  abstract  type,  but  the  following is assumed of a group
`G` of one of its concrete implementations:

  - The function `gens(G)` returns the list of generators of `G`.
  - The function `one(G)` returns the identity element of `G`.

# Examples
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

There  is a constructor of a group with arbitrary type elements, `Group(l)`
where  `l isa AbstractVector{T}` constructs a `Groupof{T}` which knows only
the general methods in this module. The examples above use
`Group(AbstractVector{<:Perm})`  which constructs  a `PermGroup`  which has
more efficient methods.

for  further information on  the functions defined  in this module, look at
the  docstrings of `Group,  gens, ngens, comm,  orbit, orbits, transversal,
words_transversal,  centralizer,  stabilizer,  center,  normalizer,  words,
minimal_words,   word,  in,   elements,  length,   order,  conjugacy_class,
conjugacy_classes, classreps, nconjugacy_classes, fusion_conjugacy_classes,
position_class,  isabelian,  iscyclic,  istrivial,  rand, transporting_elt,
intersect, Hom, kernel, Coset`
"""
module Groups
export Group, centralizer, center, order,
  classreps, class_representatives,
  comm, commutator,
  gens, generators,
  ngens, number_of_generators,
  nconjugacy_classes, number_of_conjugacy_classes,
  ordergens, orders_of_generators,
  transporting_elt, transporting_element,
  fusion_conjugacy_classes,
  conjugacy_class, conjugacy_classes, Coset, NormalCoset,
  Hom, isabelian, iscyclic, istrivial, words, minimal_words,
  normalizer, orbit, orbits,
  position_class, stabilizer,
  transversal, words_transversal, word, elements, kernel, ConjugacyClass,
  ontuples,onsets,
  getp, @GapObj

import ..Perms: orbit, orbits, order # suppress if used as indep. package
using OrderedCollections: OrderedDict
#--------------------------------------------------------------------------

"""
A  variation of get! where it is assumed f(o) sets o.p but not assumed that
f returns o.p, because f sets several keys at once...
"""
getp(f::Function,o,p::Symbol)=get!(()->(f(o);getfield(o,:prop)[p]),getfield(o,:prop),p)

"""
`@GapObj struct...`

A `GapObj` is a kind of object where properties are computed on demand when
asked  for.  So  it  has  fixed  fields  but can dynamically have new ones.
Accessing  fixed  fields  is  as  efficient  as  a  field  of any `struct`.
Accessing a dynamic field takes the time of a `Dict` lookup.

```julia_repl
julia> @GapObj struct Foo
       a::Int
       end

julia> s=Foo(1,Dict{Symbol,Any}())
Foo(1, Dict{Symbol, Any}())

julia> s.a
1

julia> haskey(s,:b)
false

julia> s.b="hello"
"hello"

julia> haskey(s,:b)
true

julia> s.b
"hello"
```
The dynamic fields are stored in the field `.prop` of `G`, which is of type
`Dict{Symbol,  Any}()`.  This  explains  the  extra  argument needed in the
constructor.  The name is because it mimics a GAP record, but perhaps there
could be a better name. The methods the `GapObj` inherits from its field
`prop` are `haskey`, `getindex`, `delete!` and `get!`.
"""
macro GapObj(e)
  push!(e.args[3].args,:(prop::Dict{Symbol,Any}))
  if e.args[2] isa Symbol T=e.args[2]
  elseif e.args[2].args[1] isa Symbol T=e.args[2].args[1]
  else T=e.args[2].args[1].args[1]
  end
  esc(Expr(:block,
   e,
   :(Base.getproperty(o::$T,s::Symbol)=hasfield($T,s) ? getfield(o,s) :
         getfield(o,:prop)[s]),
   :(Base.setproperty!(o::$T,s::Symbol,v)=hasfield($T,s) ? setfield!(o,s,v) :
         getfield(o,:prop)[s]=v),
   :(Base.haskey(o::$T,s::Symbol)=haskey(getfield(o,:prop),s)),
   :(Base.getindex(o::$T,s::Symbol)=getindex(getfield(o,:prop),s)),
   :(Base.propertynames(o::$T)=(fieldnames($T)...,Tuple(keys(getfield(o,:prop)))...)),
   :(Base.delete!(o::$T,s::Symbol)=delete!(getfield(o,:prop),s)),
   :(Base.get!(f::Function,o::$T,s::Symbol)=get!(f,getfield(o,:prop),s))))
end

#-------------- "black box groups" ------------------------------
abstract type Group{T} end # T is the type of elements of G

"`normalizer(G::Group,H::Group)` the normalizer of `H` in `G`"
function normalizer#(G::Group,H::Group)
#  error("no default implementation for normalizer")
end

"`one(G::Group)` returns the identity element of `G`."
Base.one(G::Group{T}) where T=one(T)

Base.eltype(G::Group{T}) where T=T

"`gens(G::Group)` or `generators(G::Group)` is the `Vector` of generators of `G`."
generators(G::Group)=G.gens #by default assume a concrete group has a field gens
const gens=generators

"`ngens(G::Group)` or `number_of_generators(G::Group)` is the number of generators of `G`."
number_of_generators(G::Group)=length(gens(G))
const ngens=number_of_generators

"""
`(G::Group)(i...)`

A Group used as a function takes integer arguments in `eachindex(gens(W))`.
This  constructs  the  element  of  `G`  product of the generators with the
specified  indices. An argument  can also be  negative, then the inverse of
the corresponding generator is used.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3))
Group((1,2),(1,2,3))

julia> G(2,1,-2) # returns gens(G)[2]*gens(G)[1]/gens(G)[2]
(1,3)
```
"""
function (W::Group)(w::Vararg{Integer,N})where N
  isempty(w) ? one(W) : prod(W(i) for i in w)
end

(W::Group)(i::Integer)=i>0 ? gens(W)[i] : inv(gens(W)[-i])

"`comm(a,b)` or `commutator(a,b)` is `a^-1*b^-1*a*b`"
comm(a,b)=inv(a)*inv(b)*a*b
commutator(a,b)=comm(a,b)

"""
`ontuples(t,g)`

Assume  that `t` is a  `Vector` or a `NTuple`.  `ontuples` is the action of
`g` given by `(t,g)->map(x->x^g,t)`.
"""
ontuples(t,g)=map(x->x^g,t)

"""
`onsets(s,g)`

Assume that `s` is a set, represented as a sorted list without repetitions.
`onsets` is the action of `g` given by `(s,g)->sort!(map(x->x^g,s))`.
"""
onsets(s,g)=sort!(map(x->x^g,s))

"""
`orbit(gens::AbstractVector,p,action::Function=^)`

`orbit(G::Group,p,action::Function=^)`

the orbit of point `p` under repeated action of generators `gens`. The type
of  point `p` should be hashable. The  default action of a group element is
`^`.  For example if `g` is a permutation  and `p` an integer, `p^g` is the
image  of `p` by `g`; if `h` and  `g` are group elements, then `h^g` is the
conjugate  `inv(g)*h*g`. If  a group  is given  instead of  generators, the
orbit under `gens(G)` is returned.

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
"""
function orbit(gens::AbstractVector,pnt,action::Function=^)
  set=Set([pnt])
  orb=[pnt]
  for pnt in orb, gen in gens
    img=action(pnt,gen)
    if !(img in set)
      push!(orb,img)
      push!(set,img)
    end
  end
  orb
end

orbit(G::Group,pnt,action::Function=^)=orbit(gens(G),pnt,action)

"""
`transversal(G::Group,p,action::Function=^)`

returns  an `OrderedDict` `t` with keys  `orbit(G,p,action)` and where `t[x]` is an
element  of  `G`  such  that  `x==action(p,t[x])`.  Like  `orbit`,  it thus
requires the type of `p` to be hashable.

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
"""
function transversal(G::Group,pnt,action::Function=^)
  trans=OrderedDict(pnt=>one(G))
  orb=[pnt]
  for pnt in orb, gen in gens(G)
    img=action(pnt,gen)
    if !haskey(trans,img)
      push!(orb,img)
      trans[img]=trans[pnt]*gen
    end
  end
  trans
end

function extend_transversal!(trans,G::Group,action::Function=^)
  orb=collect(keys(trans))
  for pnt in orb, gen in gens(G)
    img=action(pnt,gen)
    if !haskey(trans,img)
      push!(orb,img)
      trans[img]=trans[pnt]*gen
    end
  end
  trans
end

"""
`words_transversal(gens,p,action::Function=^)`

A   transversal   recording   words.   returns   a  `Dict`  `t`  with  keys
`orbit(gens,p,action)` and where `t[x]` is a sequence of integers such that
`x==action(p,prod(gens[t[x]]))`,  that is for each element `x` of the orbit
of `p` describes as a word in `gens` an element bringing `p` to `x`.

```julia-repl
julia> words_transversal([Perm(1,2),Perm(2,3)],1)
OrderedDict{Int64, Vector{Int64}} with 3 entries:
  1 => []
  2 => [1]
  3 => [1, 2]
```
"""
function words_transversal(gens,pnt,action::Function=^)
  trans=OrderedDict(pnt=>Int[])
  orb=[pnt]
  for pnt in orb, (i,gen) in enumerate(gens)
    img=action(pnt,gen)
    if !haskey(trans,img)
      push!(orb,img)
      trans[img]=push!(copy(trans[pnt]),i)
    end
  end
  trans
end

function orbits(gens::AbstractVector,v::AbstractVector,action::Function=^;trivial=true)
  res=Vector{eltype(v)}[]
  while !isempty(v)
    o=orbit(gens,first(v),action)
    if length(o)>1 || trivial push!(res,o) end
    v=setdiff(v,o)
  end
  res
end

"""
`orbits(gens::Vector,v,action=^;trivial=true)`

`orbits(G,v,action=^;trivial=true)`

the  orbits on `v`  of the repeated  action of `gens`;  the elements of `v`
should  be hashable. If a  group is given instead  of generators, the orbit
under  `gens(G)` is returned. If `trivial=false` the one-element orbits are
not returned.

```julia-repl
julia> G=Group(Perm(1,2),Perm(2,3));
julia> orbits(G,1:4)
2-element Vector{Vector{Int64}}:
 [1, 2, 3]
 [4]
```
"""
orbits(G::Group,v,action::Function=^;trivial=true)=orbits(gens(G),v,action;trivial)

"""
`centralizer(G::Group,p,action=^)`

computes  the subgroup of elements `g` of `G` such that `action(p,g)==p`.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3));
julia> centralizer(G,1)
Group((2,3))
```
"""
centralizer(G::Group,p,action::Function=^)=stabilizer(G,p,action)

"""
`centralizer(G::Group,H::Group)` the centralizer in `G` of the group `H`
```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3))
Group((1,2),(1,2,3))

julia> centralizer(G,Group(Perm(1,2)))
Group((1,2))
```
"""
centralizer(G::Group,H::Group)=stabilizer(G,gens(H),ontuples)

"""
`stabilizer(G::Group,s,action=^)`

computes  the subgroup of elements `g` of `G` such that `action(p,g)==p`.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3,4))
Group((1,2),(1,2,3,4))
```
Assume that `s` is a set, represented as a sorted list without repetitions.
`onsets` is the  action  of  `g∈  G`  given  by  `(g,p)->sort(p.^g)`.
```julia-repl
julia> stabilizer(G,[1,2],onsets)
Group((3,4),(1,2))
```
"""
function stabilizer(G::Group,p,action::Function=^)
  t=transversal(G,p,action)
  if length(t)==1 return G end
  C=unique(wx*s/t[action(x,s)] for (x,wx) in t for s in gens(G))
  Group(C) #Schreier generators
end

function stabilizer(G::Group,p,::typeof(ontuples))
  C=G
  for g in p C=stabilizer(C,g) end
  C
end

"""
`center(G::Group)` the center of `G`

```julia-repl
julia> G=Group(Perm(1,2),Perm(3,4),Perm(1,3)*Perm(2,4))
Group((1,2),(3,4),(1,3)(2,4))

julia> center(G)
Group((1,2)(3,4))
```
"""
function center(G::Group{T})where T
  get!(G,:center) do
    centralizer(G,G)
  end::Group{T}
end

"""
`minimal_words(G::Group)`

returns  a `Dict` giving for each element of `G` a minimal positive word in
the generators representing it.

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
"""
function minimal_words(G::Group{T})where T
  get!(G,:minwords)do
    words_transversal(gens(G),one(G),(x,y)->x*y)
  end::OrderedDict{T,Vector{Int}}
end

"""
`minimal_words(G::Group,w)`

Gives all expressions of `w` as words in the generators of `G` of
minimal length (uses `minimal_words(G)`).
```julia-repl
julia> G=Group(Perm(1,2),Perm(2,3));

julia> minimal_words(G,Perm(1,3))
2-element Vector{Vector{Int64}}:
 [1, 2, 1]
 [2, 1, 2]
```
"""
function minimal_words(G::Group,w)
  d=minimal_words(G)
  m=d[w]
  if length(m)<=1 return [m] end
  res=Vector{Int}[]
  for i in eachindex(gens(G))
    v=w*G(-i)
    if length(d[v])<length(m) 
      append!(res,push!.(copy.(minimal_words(G,v)),i)) 
    end
  end
  res
end

"""
`words(G::Group)`

returns  a `Dict`  giving for  each element  of `G`  a positive word in the
generators representing it. It is faster than `minimal_words` but the words
are not guaranteed minimal.

```julia-repl
julia> G=Group(Perm(1,2),Perm(1,2,3));
julia> words(G)
OrderedDict{Perm{Int16}, Vector{Int64}} with 6 entries:
  ()      => []
  (1,2)   => [1]
  (1,2,3) => [2]
  (1,3)   => [1, 2]
  (2,3)   => [2, 1]
  (1,3,2) => [1, 2, 1]
```
"""
function words(G::Group{T})where T
  get!(G,:words)do
    words=OrderedDict(one(G)=>Int[])
    for i in eachindex(gens(G))
      nwords=copy(words)
      rw = [one(G)=>Int[]]
      while !isempty(rw)          # look at group generated by gens(W)[1:i]
        p=popfirst!(rw)
        for k in 1:i
          e=first(p)*gens(G)[k]
          if !haskey(nwords,e)
            we=vcat(last(p),[k])
            push!(rw,e=>we)
            for (e1,w1) in words nwords[e1*e]=vcat(w1,we) end
          end
        end
      end
      words = nwords
    end
    words
  end::OrderedDict{T,Vector{Int}}
end

# returns Dict: (word w, n0 generator such that l(w/G(n0))<l(w)
# faster than words but longer to retrieve word
function words2(G::Group{T})where T
  get!(G,:words2)do
    words=OrderedDict(one(G)=>0)
    for i in eachindex(gens(G))
      nwords=copy(words)
      rw=[one(G)=>0]
      while !isempty(rw)          # look at group generated by gens(W)[1:i]
        p=popfirst!(rw)
        for k in 1:i
          e=first(p)*gens(G)[k]
          if !haskey(nwords,e)
            push!(rw,e=>k)
            for (e1,w1) in words nwords[e1*e]=k end
          end
        end
      end
      words = nwords
    end
    words
  end::OrderedDict{T,Int}
end

"`word(G::Group,w)` a minimal word in `gens(G)` representing element `w` of `G`"
word(G::Group,w)=minimal_words(G)[w]

function word2(W::Group,w)
  res=Int[]
  d=words2(W)
  while !isone(w)
    i=d[w]
    w/=gens(W)[i]
    pushfirst!(res,i)
  end
  res
end

"`elements(G::Group)` the list of elements of G"
function elements(G::Group)
  collect(keys(words2(G)))
end

"`x in G` for `G` a group: whether `x` is an element of `G`"
Base.in(w,G::Group)=haskey(words2(G),w)

"""
`length(G::Group)` the number of elements of `G`.

`length(T,G)` do the computation with the integer type `T`.
"""
Base.length(G::Group)=length(words2(G))

"""
`order(G::Group)` the number of elements of `G`.

`order(T<:Integer,G::Group)` do the computation with the integer type `T`
  (useful when a `BigInt` is needed to hold the result).
"""
order(G::Group)=length(G)
order(::Type{T},G::Group) where T<:Integer=length(T,G)

@GapObj struct ConjugacyClass{T,TW}
  G::TW
  representative::T
end

Base.eltype(C::ConjugacyClass{T,TW}) where {T,TW}=T

function Base.show(io::IO,C::ConjugacyClass)
  print(io,"ConjugacyClass(",C.G,",",C.representative,")")
end

"""
`conjugacy_classes(G::Group)` conjugacy classes of `G`
(as a `Vector{ConjugacyClass}`)
"""
function conjugacy_classes(G::Group{T})where T
  get!(G,:classes) do
    if haskey(G,:classreps)
      [ConjugacyClass(G,x,Dict{Symbol,Any}()) for x in G.classreps]
    else
      if length(G)>24000 error("length(G)=",length(G),": too big for ",typeof(G)) end
      res=orbits(G,elements(G))
      # assumes l sortable
      map(l->ConjugacyClass(G,minimum(l),Dict{Symbol,Any}(:elements=>sort(l))),res)
    end
  end
end

function elements(C::ConjugacyClass)
  get!(C,:elements)do
    orbit(C.G,C.representative)
  end::Vector{eltype(C)}
end

Base.length(C::ConjugacyClass)=length(elements(C))

Base.in(x,C::ConjugacyClass)=x in elements(C)

"`conjugacy_class(G::Group,g)` the class of `g`"
conjugacy_class(G::Group,g)=conjugacy_classes(G)[position_class(G,g)]

"`position_class(G::Group,g)` index of conjugacy class to which `g` belongs"
function position_class(G::Group,g)
  p=findfirst(==(g),classreps(G))
  if !isnothing(p) return p end
  findfirst(c->g in c,conjugacy_classes(G))
end

"""
`fusion_conjugacy_classes(H::Group,G::Group)`

A `Vector{Int}` telling for each conjugacy class of subgroup `H` of which class
of `G` is is a subset
"""
function fusion_conjugacy_classes(H::Group,G::Group)
  map(x->position_class(G,x),classreps(H))
end

"""
`class_representatives(G::Group)`  or `classreps`

representatives  of  conjugacy  classes  of  `G`.  By  default  queries the
attribute  `G.classreps`, and if this attribute  is present it will be used
by `conjugacy_classes`.
"""
function classreps(G::Group{T}) where T
  get!(G,:classreps) do
    getproperty.(conjugacy_classes(G),:representative)
  end::Vector{T}
end

const class_representatives=classreps

"""
`number_of_conjugacy_classes(G::Group)` or `nconjugacy_classes`

the number of conjugacy classes of `G`"
"""
number_of_conjugacy_classes(G::Group)=length(conjugacy_classes(G))
const nconjugacy_classes=number_of_conjugacy_classes

"`order(a)` the smallest integer `i≥1` such that `isone(a^i)`"
function order(a)# default method
  i=1
  u=a
  while true
   if isone(u) return i end
    i+=1
    u*=a
  end
end

"""
`orders_of_generators(G::Group)` or `ordergens`

The list of orders of the generators (this may be expensive to compute
so could be worth being cached in `G`).
"""
orders_of_generators(W)=get!(()->order.(gens(W)),W,:ordergens)::Vector{Int}
const ordergens=orders_of_generators

"`isabelian(G::Group)` whether `G` is abelian"
isabelian(W::Group)=all(x*y==y*x for x in gens(W), y in gens(W))

"`iscyclic(G::Group)` whether `G` is cyclic"
iscyclic(W::Group)=isabelian(W) && lcm(ordergens(W))==length(W)

"`istrivial(G::Group)` whether `G` is trivial"
istrivial(G::Group)=all(isone,gens(G))

"`rand(W::Group)` a random element of `W`"
function Base.rand(W::Group)
  if !haskey(W,:seed) W.seed=one(W) end
  W.seed *=W(rand(eachindex(gens(W)),rand(3:5))...)
  W.seed
end

"""
`transporting_elt(G,p,q,action=^)`    or
`transporting_element(G,p,q,action=^)`

returns  an  element  `g∈  G`  such  that  `p^g==q` (or `action(p,g)==q` if
`action` is given), if such a `g` exists, and nothing otherwise. The set of
possible `g` forms a right coset of the centralizer of p in G.

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
"""
function transporting_element(W::Group,x,y,action::Function=^;dist=nothing,verbose=false)
  if isnothing(dist)
    if x==y return one(W) end
    t=transversal(W,x,action)
    if haskey(t,y) return t[y] else return nothing end
  end
  p=one(W)
  if verbose print(dist(x, y), " ") end
  x1=x
  while true
    prev=dist(x1, y)
    if prev==0
      if verbose print("\n") end
      return p
    end
    dmin=minimum(map(g->(dist(action(x1, g), y),g),gens(W)))
    if dmin[1]<prev
      if verbose print("->",dmin) end
      p*=dmin[2]
      x1=action(x1,dmin[2])
    else
      if verbose print("\n[ stalled -- restarting at a random element of",
                      "W(size $(length(W)))\n") end
      p*=rand(W)
      x1=action(x,p)
    end
  end
end

const transporting_elt=transporting_element

# suppress unneeded generators
function weedgens(G::Group)
  l=length(G)
  gen=empty(gens(G))
  if l==1 return Group(gen) end
  lr=1
  for g in gens(G)
    N=Group(vcat(gen,[g]))
    ln=length(N)
    if ln==l return N end
    if ln>lr
      lr=ln
      gen=vcat(gen,[g])
    end
  end
end

"`intersect(G::Group, H::Group)` the intersection as a group"
function Base.intersect(G::Group, H::Group) # horrible implementation
  if all(x->x in H,gens(G)) return G end
  if all(x->x in G,gens(H)) return H end
  if min(length(G),length(H))>104000 error("too large intersect($G,$H)") end
  if length(G)<length(H) res=Group(filter(x->x in H,elements(G)))
  else res=Group(filter(x->x in G,elements(H)))
  end
  weedgens(res)
end

#------------------- homomorphisms ----------------------------------------
"""
`Hom(S::Group,T::Group,images)`

builds an object representing the homomorphism from `S` to `T` which maps
`gens(S)` to `images`.

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
"""
struct Hom{T,T1}
  source::Group{T}
  target::Group{T1}
  images::Vector{T1}
end

function Base.show(io::IO,h::Hom)
  if h.source==h.target
    print(io,"Aut(",h.source,";",gens(h.source),"↦ ",h.images)
  else
    print(io,"Hom(",h.source,"→ ",h.target,";",gens(h.source),"↦ ",h.images)
  end
end

"`kernel(h::Hom)` the kernel of the homomorphism `h`"
function kernel(h::Hom)
  if all(isone,h.images) return h.source
  elseif length(h.source)==length(Group(h.images))
    return Group(empty(gens(h.source)),one(h.source))
  elseif length(h.source)<1000
    return Group(filter(x->isone(h(x)),elements(h.source)))
  else error("not implemented: kernel(",h,")")
  end
end

"If `h isa Hom` then `h(w)` is the image of `w` by `h`"
(h::Hom)(w)=isone(w) ? one(h.target) : prod(
  (i>0 ? h.images[i] : inv(h.images[-i])) for i in word(h.source,w))

#------------------- "abstract" concrete groups -------------------------------
@GapObj struct Groupof{T}<:Group{T}
  gens::Vector{T}
  one::T
end

"""
`Group(l::AbstractVector{T}[,one]) where T`

A  group may be constructed  from a list of  `l` elements of the same type.
These  elements must respond to  the functions `*` and  `inv`. If it is not
possible  to compute  `one` from  `l` (because  `l[1]` does  not respond to
`one`,  or  `l`  is  empty  and  `T`  does  not respond to `one`), then the
identity element of the group must be given as a second argument.

```julia-repl
julia> G=Group([[-1 -1;1 0]])
Group([[-1 -1; 1 0]])

julia> elements(G)
3-element Vector{Matrix{Int64}}:
 [1 0; 0 1]
 [-1 -1; 1 0]
 [0 1; -1 -1]
```
"""
function Group(a::AbstractVector{T}) where T
  Groupof(filter(!isone,a),!isempty(a) ? one(a[1]) : one(T),Dict{Symbol,Any}())
end

# for the case one(a::T) is defined but not one(T)
function Group(a::AbstractVector{T},one) where T
  Groupof(filter(!=(one),a),one,Dict{Symbol,Any}())
end

Base.one(G::Groupof)=G.one

function Base.show(io::IO,G::Groupof)
  print(io,"Group(",gens(G),")")
end

#------------------- cosets ----------------------------------------
abstract type Coset{T,TW<:Group{<:T}} end
# assumed to have a method Group and a field phi: represents Group.phi

Base.isone(a::Coset)=a.phi in Group(a)

Base.eltype(G::Coset{T,TW}) where {T,TW}=T

Group(W::Coset)=W.G

# my cosets are right cosets
(W::Coset)(x...)=Group(W)(x...)*W.phi

Base.cmp(a::Coset, b::Coset)=cmp(a.phi,b.phi)

Base.isless(a::Coset, b::Coset)=cmp(a,b)==-1

Base.:(==)(a::Coset, b::Coset)=(Group(a)==Group(b)) && (a.phi/b.phi in Group(a))

Base.hash(a::Coset, h::UInt)=hash(a.phi,h) # quotient groups will not work

Base.copy(C::Coset)=Coset(Group(C),C.phi)

Base.one(C::Coset)=Coset(Group(C))

Base.length(C::Coset)=length(Group(C))

Base.show(io::IO,C::Coset)=print(io,Group(C),".",C.phi)

elements(C::Coset)=elements(Group(C)).*Ref(C.phi)

Base.in(w,C::Coset)=C.phi/w in Group(C)

abstract type NormalCoset{T,TW<:Group{<:T}}<:Coset{T,TW} end

# for now only normal cosets (phi normalizes G), the minimum for quotient groups
Base.inv(C::NormalCoset)=NormalCoset(Group(C),inv(C.phi))

Base.:*(a::NormalCoset,b::NormalCoset)=NormalCoset(Group(a),a.phi*b.phi)

Base.:/(a::NormalCoset,b::NormalCoset)=a*inv(b)

Base.:^(a::NormalCoset, n::Integer)=n>=0 ? Base.power_by_squaring(a,n) :
                                     Base.power_by_squaring(inv(a),-n)

Base.:^(a::NormalCoset, b::NormalCoset)= inv(b)*a*b

function order(a::NormalCoset)
  i=1
  u=a.phi
  while true
    if u in Group(a) return i end
    i+=1
    u*=a.phi
  end
end

Base.copy(C::NormalCoset)=NormalCoset(Group(C),C.phi)

Base.one(C::NormalCoset)=NormalCoset(Group(C))

# assume H is normal and there is a function NormalCoset
Base.:/(W::Group,H::Group)=Group(unique(map(x->NormalCoset(H,x),gens(W))),NormalCoset(H))

function classreps(G::NormalCoset)
  get!(G,:classreps) do
    getproperty.(conjugacy_classes(G),:representative)
   end::Vector{eltype(G)}
end

nconjugacy_classes(G::NormalCoset)=length(classreps(G))

function elements(C::ConjugacyClass{T,TW})where {T,TW<:Coset}
  get!(C,:elements)do
    orbit(Group(C.G),C.representative)
  end::Vector{T}
end

function conjugacy_classes(G::NormalCoset)
  get!(G,:classes) do
    if haskey(G,:classreps)
      [ConjugacyClass(G,x,Dict{Symbol,Any}()) for x in G.classreps]
    else res=orbits(Group(G),elements(G))
      res=map(l->ConjugacyClass(G,minimum(l),Dict{Symbol,Any}(:elements=>sort(l))),res)
      G.classreps=getproperty.(res,:representative)
      res
    end
  end
end

function position_class(G::NormalCoset,g)
  p=findfirst(==(g),classreps(G))
  if !isnothing(p) return p end
  findfirst(c->g in c,conjugacy_classes(G))
end

# assume H is a subcoset of G
function fusion_conjugacy_classes(H::NormalCoset,G::NormalCoset)
  map(x->position_class(G,x),classreps(H))
end

@GapObj struct Cosetof{T,TW<:Group{<:T}}<:Coset{T,TW}
  phi::T
  G::TW
end

"""
`Coset(G::Group,phi=one(G))`  constructs the (left)  coset `G.phi` where `G
isa  Group{<:T}` and `phi isa  T`, as an object  of type `Cosetof{T}`. This
general  coset knows only the general methods for a coset `C=G.phi` defined
in this module, which are

  - `Group(C)` returns `G`.
  - `isone(C)` returns `true` iff `phi in G`
  - `one(C)` returns the trivial coset `G.1`
  - `length(C)` returns `length(G)`
  - `elements(C)` returns `elements(G).*Ref(phi)`
  - `x in C` returns `x/phi in G`
"""
Coset(G::Group,phi=one(G))=Cosetof(phi,G,Dict{Symbol,Any}())

@GapObj struct NormalCosetof{T,TW<:Group{<:T}}<:NormalCoset{T,TW}
  phi::T
  G::TW
end

"""
`NormalCoset(G::Group,phi=one(G))`  constructs the coset `C=G.phi` where `G
isa  Group{<:T}` and `phi isa T`,  as an object of type `NormalCosetof{T}`.
It  is assumed that `phi` normalizes `G`. This general coset knows only the
general  methods defined for normal cosets in the module `Groups`, which in
addition to those defined for cosets (see `Coset`) are

  - `inv(C)` return `G.inv(phi)` (assumed equal to `inv(phi).G`)
  - `C*D` given another coset `G.psi` returns `G.phi*psi`
  - `C/D` given another coset `G.psi` returns `G.phi*inv(psi)`
  - `C^D` given another coset `G.psi` returns `G.inv(psi)*phi*psi`
  - `C^n` returns `G.phi^n`
  - `order(C)` the smallest `n` such that `isone(C^n)`

The  conjugacy  classes  of  a  normal  coset  `G.phi`  are relative to the
conjugation action of `G` on `G.phi`. We have the functions
`conjugacy_classes, nconjugacy_classes, classreps, position_class`.

Finally  the function  `G/H` for  two groups  constructs the  quotient as a
group of `NormalCoset`s, and
`fusion_conjugacy_classes(H::NormalCoset,G::NormalCoset)`   expresses   the
fusion of conjugacy classes.
"""
NormalCoset(G::Group,phi=one(G))=NormalCosetof(phi,G,Dict{Symbol,Any}())

end
