"""
This  module is a port  of some GAP functionality  on permutation groups. A
`PermGroup` is a `Group` where `gens` are `Perm`s. It depends on the modules
`Groups` and `Perms` which could be independent packages on their own.

```julia-repl
julia> G=Group([Perm(i,i+1) for i in 1:2])
Group((1,2),(2,3))

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
1-element Vector{Vector{Int16}}:
 [1, 2, 3]

julia> Perm(1,2) in G
true

julia> Perm(1,2,4) in G
false
```
`elements`,   `in`  and   other  functions   are  computed   on  `G`  using
Schreier-Sims theory, that is computing the following
```julia-repl
julia> get_stabchain(G)
b:1 c:Group((1,2),(2,3))
  δ:1=>(),2=>(1,2),3=>(1,3,2)
b:2 c:Group((2,3))
  δ:2=>(),3=>(2,3)
```
See the docstring of `stabchain` for explanations.

There are efficient methods for `PermGroups` for the functions `in, length,
elements,   position_class`.  The  function   `on_classes`  determines  the
permutation  of the conjugacy classes effected by an automorphism. Finally,
we   give  application  to  the  group   of  simultaneous  row  and  column
permutations of a matrix: see `onmats, stab_onmats, Perm`.

finally, benchmarks on julia 1.8
```benchmark
julia> @btime collect(symmetric_group(8));
  2.673 ms (129965 allocations: 5.76 MiB)

julia> @btime words(symmetric_group(8));
  7.155 ms (86019 allocations: 11.00 MiB)

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
Note  the use of  `Int128` in `length`:  the computation does  not fit in an
`Int64`.
"""
module PermGroups
using Reexport
include("Perms.jl"); @reexport using .Perms
include("Groups.jl"); @reexport using .Groups
using Combinat: tally, collectby
export PermGroup, symmetric_group, reduced, stab_onmats, onmats, on_classes,
       get_stabchain, stabchain, Stabchain, Stablink
#-------------------- now permutation groups -------------------------
abstract type PermGroup{T}<:Group{Perm{T}} end

PermGroup()=Group(Perm{Int16}[])

function Base.show(io::IO,G::PermGroup{T})where T
  print(io,"Group(")
  t=isempty(gens(G)) || T!=Perms.Idef 
  if t print(io,"Perm{",T,"}[") end
  join(io,gens(G),",")
  if t print(io,"]") end
  print(io,")")
end

Base.one(G::PermGroup)=G.one # PermGroups should have fields gens and one

"`largest_moved_point(G::PermGroup)` the largest moved point by any `g∈ G`"
function Perms.largest_moved_point(G::PermGroup{T})where T
  get!(G,:largest_moved)do
    maximum(largest_moved_point.(gens(G));init=T(0))
  end::T
end

" `orbits(G::PermGroup)` the orbits of `G` on its moved points."
function Perms.orbits(G::PermGroup{T})where T
  get!(G,:orbits)do
   orbits(G,T(1):largest_moved_point(G);trivial=false)
  end::Vector{Vector{T}}
end

struct SchreierTransversal{T<:Signed,TP<:Perm{T}}
  v::Vector{T}
  gg::Vector{TP}
end

"""
describe the orbit of Int p under PermGroup G as a Schreier vector v.
That is, v[p]==-1 and v[k]=i means that k^inv(G(i)) is the antecessor of k
in the orbit of p.
"""
function SchreierTransversal(G::PermGroup{T},p::Integer)where T
# T should be a signed type
  res=zeros(T,max(largest_moved_point(G),p))
  res[p]=-1
  new=[p]
  while true
    n=copy(new)
    empty!(new)
    for p in n, i in eachindex(gens(G))
      q=p^gens(G)[i]
      if res[q]==0
        res[q]=i
        push!(new,q)
      end
    end
    if isempty(new) break end
  end
  SchreierTransversal(res,gens(G))
end

function Groups.extend_transversal!(t::SchreierTransversal,G::PermGroup)
  new=filter(!iszero(t.v[i]),eachindex(t.v))
  append!(t.gg,setdiff(gens(g),t.gg))
  while true
    n=copy(new)
    empty!(new)
    for p in n, i in eachindex(t.gg)
      q=p^t.gg[i]
      if t[q]==0
        t[q]=i
        push!(new,q)
      end
    end
    if isempty(new) break end
  end
  t
end

Base.haskey(t::SchreierTransversal,k::Integer)=k<=length(t.v) && !iszero(t.v[k])

function Base.getindex(t::SchreierTransversal,k::Integer)
  p=t.v[k]
  if p==-1 return one(first(t.gg)) end
  if iszero(p) error(k," not in orbit") end
  g=t.gg[p]
  res=g
  while true
    k=preimage(k,g)
    p=t.v[k]
    if p==-1 return res end
    if iszero(p) error(k," not in orbit") end
    g=t.gg[p]
    res=g*res
  end
end

Base.length(t::SchreierTransversal)=count(!iszero,t.v)

function Base.iterate(t::SchreierTransversal)
  i=findfirst(!iszero,t.v)
  (i=>t[i],i)
end

function Base.iterate(t::SchreierTransversal,i)
  i=findnext(!iszero,t.v,i+1)
  if isnothing(i) return end
  (i=>t[i],i)
end

struct Stablink{T,TG<:PermGroup{T},TT}
  b::T
  c::TG
  δ::TT
end

"""
A  `Stabchain` is  a  `Vector{Stablink}` `S=[S₁,…,Sₖ]` associated to a
`PermGroup{T}`  `G`. The `Vector{T}` given by `B=[S₁.b,…,Sₖ.b]` is called a
`base`   for  `G`.   At  each   stage  one   has  `Sᵢ.c=C_G(B[1:i-1])`  and
`Sᵢ.δ=transversal(Sᵢ.c,Sᵢ.b)`
"""
const Stabchain{T,TG,TT}=Vector{<:Stablink{T,TG,TT}}

base(S::Stabchain)=map(s->s.b,S)

function Base.show(io::IO,S::Stabchain)
  print(io,"[")
  for i in 1:length(S)
    print(io,"Stablink(",S[i].b,",",S[i].c,",\n  ",S[i].δ,")")
    if i!=length(S) print(io,",\n") end
  end
  print(io,"]")
  return
end

function Base.show(io::IO, ::MIME"text/plain", S::Stabchain)
  if !get(io,:limit,false) show(io,S);return end
  n=maximum(s->maximum(keys(s.δ)),S;init=0)
  ord=invperm(vcat(base(S),setdiff(1:n,base(S))))
  for i in eachindex(S)
    println(io,"b:",S[i].b," c:",S[i].c)
    print(io,"  δ:")
    l=sort(collect(keys(S[i].δ)),by=i->ord[i])
    join(io,[string(j,"=>",repr(S[i].δ[j],context=io)) for j in l],",")
    if i!=length(S) println(io) end
  end
end

"""
`strip(g::Perm,S::Stabchain)`

returns g "stripped" of its components in all `S.c`, that is a pair 
(an element which fixes S[1:i-1].b and sends S.b[i] outside S.δ[i],i+1)
"""
function strip(g::Perm,S::Stabchain)
  g=copy(g)
  for (i,S) in enumerate(S)
    β=S.b^g
    if !haskey(S.δ,β) return g,i end
    Perms.mul!(g,inv(S.δ[β]))
  end
  g,length(S)+1
end

"""
Constructs a `Stabchain` for `G`

The  code refers  to the  Handbook of  computational group theory, by Holt,
Eick,  O'Brien, section 4.4.2.
"""
function stabchain(G::PermGroup{T},B=T[];trans=transversal,weed=true)where T
  B=T.(B) # check type and make copy to be able to extend it
  for x in gens(G) 
    if all(b->b^x==b,B) push!(B,smallest_moved_point(x)) end
  end
  S=Stablink{T,PG{T},trans==transversal ? Dict{T,Perm{T}} : trans{T,Perm{T}}}[]
  for i in eachindex(B)
    C=Group(filter(g->all(b->b^g==b,B[1:i-1]),gens(G)))
    t=trans(C,B[i])
    push!(S,Stablink(B[i],C,t))
  end
  i=length(S)
  while i>=1
    for (β,wβ) in S[i].δ, x in gens(S[i].c)
      h=wβ*x/S[i].δ[β^x] # possibly new elt of C_G(B[1:i])
      if isone(h) continue end
      h,j=strip(h,S)
      if isone(h) continue end
      for l in i+1:j # now h is in C[l] for those l
        if l>length(S)
          b=smallest_moved_point(h)
          c=Group(h)
          push!(S,Stablink(b,c,trans(c,b)))
        else
          push!(gens(S[l].c),h)
          Groups.extend_transversal!(S[l].δ,S[l].c)
        end
      end
      i=j
      @goto nexti
    end
    i-=1
    @label nexti
  end
  if weed
    I=filter(eachindex(S))do i
      if istrivial(S[i].c) return false end
      if length(S[i].δ)==1 return false end
      true
    end
#   if length(S)-length(I)>5
#     @show G,B
#   end
    S=S[I]
  end
  S
end

"This function adds to G a stabchain"
function get_stabchain(G::PermGroup{T})where T
  get!(G,:stabchain)do
    stabchain(G,T[])
  end::Vector{Stablink{T,PG{T},Dict{T,Perm{T}}}}
end

function Base.in(g::Perm,G::PermGroup)
  g,i=strip(g,get_stabchain(G))
  isone(g)
end

Groups.stabilizer(G::PermGroup,p::Integer)=stabilizer(G,[p],ontuples)

function Groups.stabilizer(G::PermGroup,p::AbstractVector{<:Integer},::typeof(ontuples))
  S=stabchain(G,p)
  p=findfirst(s->!(s.b in p),S)
  if isnothing(p) return Group(one(G)) end
  S[p].c
end

"""
`search(S::Stabchain,test,property;all=true)`
find  one (all if  `all==true`) element `g`  of the group  generated by `S`
statisfying `property(g)`. The fuction `test(g,l)` should return `false` if
it can prove that `property(g)` is `false` by looking at `base(S)[1:l].^g`.

See [Holt] page 114.
"""
# test(g,l) should work by consulting base(S)[1:l]
function search(S::Stabchain{T},test,property;all=true)where T
  if isempty(S) 
    g=one(Perm{T})
    if property(g) return all ? [g] : g end
    return nothing
  end
  base=map(s->s.b,S)
  n=max(maximum(base),maximum(s->largest_moved_point(s.c),S))
  ord=invperm(vcat(base,setdiff(1:n,base)))
  k=length(S)
  c=fill(0,k)
  Λ=fill(Int[],k)
  u=[one(Perm{T}) for i in 1:k]
  Δ=map(s->sort(collect(keys(s.δ)),by=i->ord[i]),S)
  l=1;c[l]=1;Λ[l]=Δ[l];u[l]=one(Perm{T})
# @show Λ
  res=Perm{T}[]
  while true
    gl=prod(u[l:-1:1])
    while l<k && test(gl,l)
      l+=1
      Λ[l]=sort(Δ[l].^gl,by=i->ord[i])
      c[l]=1
      γ=preimage(Λ[l][c[l]],gl)
      u[l]=S[l].δ[γ]
      gl=u[l]*gl
    end
#   @show gl,c
    if l==k && test(gl,k) && property(gl) 
      if !all return gl
      else push!(res,gl)
      end
    end
    while l>0 && c[l]==length(S[l].δ) 
      l-=1
    end
    if l==0 return all ? res : nothing end
    c[l]+=1
    γ=Λ[l][c[l]]
    for k in 1:l-1
      γ=preimage(γ,u[k])
    end
    u[l]=S[l].δ[γ]
  end
end

const debug=Ref(false)

function Groups.stabilizer(W::PermGroup,b::AbstractVector{<:Integer},::typeof(onsets))
  if debug[] println("using stabilizer(PG,Vec{I},onsets)") end
  if length(b)<=2
    res=stabilizer(W,b,ontuples)
    if length(b)==2
      p=transporting_elt(W,b,reverse(b),ontuples)
      if !isnothing(p) push!(gens(res),p) end
    end
    return Groups.weedgens(res)
  end
  b=sort(b)
  S=stabchain(W,b)
  test(g,l)=all(i->b[i]^g in b,1:min(l,length(b)))
  property(g)=onsets(b,g)==b
  Groups.weedgens(Group(search(S,test,property)))
end

if false
function Groups.stabilizer(W::PermGroup,g::Perm)
  if debug[] println("using stabilizer(PG,Perm)") end
  cc=orbits(g,1:largest_moved_point(W),trivial=true)
  sort!(cc,by=length)
  d=Dict{Int,Tuple{Int,Int,Int}}()
  for i in eachindex(cc), (j,k) in enumerate(cc[i])
    d[k]=(length(cc[i]),i,j)
  end
  S=stabchain(W,vcat(cc...))
  b=base(S)
  function test(h,l)
    if l>length(b) return test(h,l-1) end
    if d[b[l]^h][1]!=d[b[l]][1] return false end
    nb=Set()
    nhb=Set()
    for j in 1:l-1
      push!(nb,d[b[j]][2])
      push!(nhb,d[b[j]^h][2])
      if length(nb)!=length(nhb) return false end
      if d[b[j]][2]==d[b[l]][2] && 
       ((d[b[j]][3]-d[b[l]][3])-(d[b[j]^h][3]-d[b[l]^h][3]))%d[b[j]][1]!=0
        return false
      end
    end
    if l>1 return test(h,l-1) end
    return true
  end
  property(h)=g^h==g
  Groups.weedgens(Group(search(S,test,property)))
end
end

#function Groups.stabilizer(W::PermGroup,p::AbstractVector{<:Perm},::typeof(onsets))
#  if debug[] println("using stabilizer(PG,Vec{Perm},onsets)") end
#  S=get_stabchain(W)
#  b=base(S)
#  test(h,l)=all(g1->any(g->all(i->b[i]^(inv(h)*g1)==b[i]^(g*inv(h)),1:min(l,length(b))),p),p)
#  property(g)=onsets(p,g)==p
#  res=Group(search(S,test,property))
#  Groups.weedgens(res)
#end

function Groups.transporting_elt(W::PermGroup,b::AbstractVector{<:Integer},b1::AbstractVector{<:Integer},::typeof(ontuples))
  if debug[] println("using transp(PG,Vec{Int},Vec{Int},ontuples)") end
  S=stabchain(W,b)
  test(g,l)=all(i->b[i]^g==b1[i],1:min(l,length(b)))
  property(g)=ontuples(b,g)==b1
  search(S,test,property;all=false)
end

function Groups.transporting_elt(W::PermGroup,b::AbstractVector{<:Integer},b1::AbstractVector{<:Integer},::typeof(onsets))
  if debug[] println("using transp(PG,Vec{Int},Vec{Int},onsets)") end
  S=stabchain(W,b)
  test(g,l)=all(i->b[i]^g in b1,1:min(l,length(b)))
  property(g)=onsets(b,g)==b1
  search(S,test,property;all=false)
end

#function Groups.transporting_elt(W::PermGroup,g::Perm,g1::Perm)
#  if debug[] println("using transp(PG,perm,perm)") end
#  S=get_stabchain(W)
#  b=base(S)
#  test(h,l)=all(i->b[i]^(inv(h)*g)==b[i]^(g1*inv(h)),1:min(l,length(b)))
#  property(h)=g^h==g1
#  search(S,test,property;all=false)
#end

# only difference with general method is sorting
function Groups.elements(C::ConjugacyClass{T,TW})where{T,TW<:PermGroup}
  sort(orbit(C.G,C.representative))
end

" The cycle types of conjugacy class `C` on each orbit of `C.G`"
function cycletypes(C::ConjugacyClass{T,TW})where{T,TW<:Union{Group{<:Perm},NormalCoset{<:Perm,<:Group}}}
  get!(C,:cycletypes)do
    cycletypes(C.G,C.representative)
  end::Vector{Vector{Int}}
end

cycletypes(W::Union{Group{<:Perm},NormalCoset{<:Perm,<:Group}},x)=
  map(o->cycletype(x,domain=o,trivial=true),orbits(W)) # first invariant

# internal function returning possibly ambiguous result
function positions_class(W::Union{Group{<:Perm},NormalCoset{<:Perm,<:Group}},w)
  ct=cycletypes(W,w)
  cl=conjugacy_classes(W)
  l=findall(C->cycletypes(C)==ct,cl)
  if length(l)==1 return l end
  if W isa Coset Z=centralizer(center(Group(W)),W.phi)
  else Z=center(W) end
  for c in filter(!isone,elements(Z))
    l=filter(i->cycletypes(W,cl[i].representative.*c)==cycletypes(W,w*c),l)
    if length(l)==1 return l end
  end
  l
end

function Groups.position_class(W::PermGroup,w)
  l=positions_class(W,w)
  if length(l)==1 return only(l) end
  for i in eachindex(l)
    if w in conjugacy_classes(W)[l[i]] return l[i] end
  end
end

"""
`on_classes(G, aut)`

`aut`  is an automorphism of  the group `G` (for  a permutation group, this
could  be  given  as  a  permutation  normalizing  `G`).  The result is the
permutation of `1:nconjugacy_classes(G)` induced by `aut`.

```julia-repl
julia> W=Group(Perm(1,2),Perm(2,3),Perm(4,5),Perm(5,6))
Group((1,2),(2,3),(4,5),(5,6))

julia> on_classes(W,Perm(1,4,2,5,3,6))
Perm{Int64}: (2,4)(3,7)(6,8)
```
"""
on_classes(G, aut)=Perm(map(c->position_class(G,c^aut),classreps(G)))

function Base.in(C::ConjugacyClass{T,TW},w::T)where{T,TW<:PermGroup}
  r=searchsortedfirst(elements(C),w)
  r<=length(C) && elments(C)[r]==w
end

#-------------- iteration on product of lists of group elements ---------------
"""
A  `ProdIterator([i₁,…,iₙ])`  takes  a  list  `i₁,…,iₙ`  of  iterators  and
iterates  on all the products `i₁[j₁]*…*iₙ[jₙ]`  (where the inner loop `jₙ`
runs  the  fastest).  It  tries  to  be  fast  by re-using partial products
`i₁[j₁]*…*iₖ[jₖ]` for `k<n`.

It is used internally for iterating over a permutation group.
"""
struct ProdIterator{T}
  iterators::Vector{T}
end

Base.length(I::ProdIterator)=prod(length.(I.iterators);init=0)
Base.eltype(::ProdIterator{T}) where T=eltype(T)

function Base.iterate(I::ProdIterator)
  T=eltype(eltype(I.iterators))
  state=Tuple{T,Int}[]
  p=findfirst(!isempty,I.iterators)
  if p===nothing n=one(T) # the best we can do in this case
  else n=one(first(I.iterators[p]))
  end
  for it in I.iterators
    u=iterate(it)
    if u===nothing return u end
    n*=first(u)
    push!(state, (n,last(u)))
  end
  return n,state
end

function Base.iterate(I::ProdIterator,state)
  for i in length(state):-1:1
    u=iterate(I.iterators[i],last(state[i]))
    if u===nothing continue end
    state[i]= i==1 ? u : (first(state[i-1])*first(u),last(u))
    for j in i+1:length(state)
      u=iterate(I.iterators[j])
      if u===nothing error() end
      state[j]=(first(state[j-1])*first(u),last(u))
    end
    return first(state[end]),state
  end
end

#------------------------- iteration for PermGroups -----------------------
"""
`length(G::PermGroup;type=Int)`  the  number  of  elements of `G`, computed
with  integers of type `type` (use `Int128` or `BigInt` for big permutation
groups).
"""
function Base.length(::Type{T},G::PermGroup)where T
  T(get!(G,:length)do
     prod(x->T(length(x.δ)),get_stabchain(G);init=1)
  end)
end

Base.length(G::PermGroup)=length(Int,G)

# iterating I directly is 25% faster unfortunately
function Base.iterate(G::PermGroup{T}) where T
  I=ProdIterator(map(x->values(x.δ),reverse(get_stabchain(G))))
  u=iterate(I)
  if u===nothing return u end
  p,st=u
  p,(I,st)
end

function Base.iterate(G::PermGroup,(I,state))
  u=iterate(I,state)
  if u===nothing return u end
  p,st=u
  p,(I,st)
end

# elements is twice faster than collect(G), should not be
function Groups.elements(G::PermGroup)
  get!(G,:elements)do
    t=reverse(sort.(collect.(values.(map(x->x.δ,get_stabchain(G))))))
    if isempty(t) return [one(G)] end
    res=t[1]
    for i in 2:length(t)
      res=vcat(map(x->res.*x,t[i])...)
    end
    res
   end::Vector{eltype(G)}
end

#------------------------- cosets for PermGroups -----------------------

# computes "canonical" element of W.phi
function reduced(W::PermGroup,phi)
  for C in get_stabchain(W)
    (kw,e)=minimum((k^phi,e) for (k,e) in C.δ)
    phi=e*phi
  end
  phi
end

# the next def will make quotient groups work
function Groups.NormalCoset(W::PermGroup,phi::Perm=one(W))
  Groups.NormalCosetof(reduced(W,phi),W,Dict{Symbol,Any}())
end

function Perms.largest_moved_point(G::NormalCoset{<:Perm{T},<:Group})where T
  get!(G,:largest_moved)do
    max(largest_moved_point(Group(G)),largest_moved_point(G.phi))
  end::T
end

function Perms.orbits(G::NormalCoset{<:Perm,<:Group})
  get!(G,:orbits)do
    v=vcat([G.phi],gens(Group(G)))
    orbits(v,1:largest_moved_point(G);trivial=false)
  end::Vector{Vector{Int}}
end

#-------------------------- now a concrete type-------------------------
@GapObj struct PG{T}<:PermGroup{T}
  gens::Vector{Perm{T}}
  one::Perm{T}
end

function Groups.Group(a::AbstractVector{Perm{T}},one=one(Perm{T})) where T
  PG(filter(!isone,a),one,Dict{Symbol,Any}())
end
Groups.Group(a::Perm...)=Group(collect(a))

" `symmetric_group(n::Int)`  The symmetric group of degree n"
symmetric_group(n::Int)=Group([Perm(i,i+1) for i in 1:n-1])

#---------------- application to matrices ------------------------------
"""
`onmats(m::AbstractMatrix,g::Perm)` synonym for `permute(m,g;dims=(1,2))`
or `permute(m,g,g)`.
"""
onmats(m,g)=permute(m,g,g)

function invblocks(m,extra=nothing)
  if isnothing(extra) extra=zeros(Int,size(m,1)) end
  blk1=[collect(axes(m,1))]
  while true
    blk=blk1
    blk1=vcat(map(I->collectby(map(i->
                 (tally(m[i,I]),tally(m[I,i]),m[i,i],extra[i]),I),I), blk)...)
    if blk==blk1 return blk end
  end
end

# fast method for centralizer(g,M,onmats) additionally centralizing extra
function stab_onmats(g::PermGroup{T},M::AbstractMatrix;extra=nothing)where T
# if length(g)>1
#   print("g=",
#        length.(filter(x->length(x)>1,orbits(g,1:maximum(degree.(gens(g)))))),
#         " M:",size(M))
# end
  blocks=invblocks(M,extra)
  for r in blocks g=stabilizer(g,r,(p,g)->sort(p.^g)) end
  if isempty(gens(g)) return g end
  n=vec(CartesianIndices(M))
  e=Group(map(y->Perm(n,map(x->CartesianIndex(x.I.^y),n)),gens(g)))
  for s in collectby(vec(M),1:length(M)) e=stabilizer(e,s,(p,g)->sort(p.^g)) end
  if isempty(gens(e)) return e end
  Group(map(p->Perm{T}(map(i->n[i^p][1], axes(M,1))), gens(e)))
end

"""
`stab_onmats([G,]M;extra=nothing)`

If  `onmats(m,p)=^(M,p;dims=(1,2))`, and  the argument  `G` is given (which
should   be  a  `PermGroup`)   this  is  just   a  fast  implementation  of
`centralizer(G,M,onmats)`.   If  `G`   is  omitted   it  is   taken  to  be
`symmetric_group(size(M,1))`.  The  program  uses sophisticated algorithms,
and  can handle matrices up to 80×80. If a list `extra` is given the result
centralizes also `extra`.

```julia-repl
julia> stab_onmats((1:30)'.*(1:30).%15)
Group((10,25),(5,20),(12,27),(3,18),(9,24),(6,21),(13,28),(8,23),(7,22),(2,17),(14,29),(11,26),(4,19),(1,4)(2,8)(3,12)(6,9)(7,13)(11,14)(16,19)(17,23)(18,27)(21,24)(22,28)(26,29),(1,11)(2,7)(4,14)(5,10)(8,13)(16,26)(17,22)(19,29)(20,25)(23,28),(1,16),(15,30))
```
"""
function stab_onmats(M::AbstractMatrix;extra=nothing,verbose=false)
  k=length(M)
  blocks=sort(invblocks(M,extra),by=x->-length(x))
  g=PermGroup()
  I=Int[]
  for r in blocks
    if length(r)>7 && verbose println("Large Block:$r")  end
    if length(r)>1
      gr=stab_onmats(symmetric_group(length(r)), M[r,r])
      g=Group(vcat(gens(g),gens(gr).^mappingPerm(eachindex(r),r)))
    end
    append!(I,r)
    p=mappingPerm(I,eachindex(I))
    g=stab_onmats(Group(gens(g).^p), M[I,I])
    g=Group(gens(g).^inv(p))
  end
  return g
end

"""
`Perm_onmats(M, N[, m ,n])`

returns `p` such that `onmats(M,p)=N` if it exists, `nothing` otherwise; so
is just an efficient version of
`transporting_elt(symmetric_group(size(M,1)),M,N,onmats)`  If  in  addition
the vectors `m` and `n` are given, `p` should satisfy `permute(m,p)=n`.
"""
function Perm_onmats(M,N,m=nothing,n=nothing;verbose=false)
  if isnothing(m) && M==N return Perm() end
  if size(M,1)!=size(M,2) || size(N,1)!=size(N,2)
    error("matrices are  not  square")
  end
  if size(M,1)!=size(N,1)
    @info "matrices do not have same dimensions"
    return nothing
  end
  sg=n->n==1 ? PermGroup() : Group(vcat(map(i->map(j->Perm(i,j),i+1:n),1:n-1)...))
  function ind(I,J)
    iM=map(i->[tally(M[i,I]), tally(M[I,i]), M[i,i]], I)
    iN=map(i->[tally(N[i,J]), tally(N[J,i]), N[i,i]], J)
    if !isnothing(m)
      iM=map(push!,iM,m[I])
      iN=map(push!,iN,n[J])
    end
    if tally(iM)!=tally(iN) return false end
    iM=collectby(iM,J)
    iN=collectby(iN,I)
    p=map(function(I,J)
      if length(I)>7
        if verbose println("large block size $(length(I))") end
        if length(iM)==1
          p=transporting_elt(sg(length(I)),M[I,I],N[J,J],
                onmats,dist=(M,N)->sum(x->count(!iszero,x),M-N))
        elseif isnothing(m) p=Perm_onmats(M[I,I],N[J,J])
        else p=Perm_onmats(M[I,I],N[J,J],m[I],n[J])
        end
      else
        p=transporting_elt(sg(length(I)),M[I,I],N[J,J],onmats)
      end
      if isnothing(p) return false end
      I=permute(I,p)
      p=mappingPerm(eachindex(I), I)
      return [I,J,Group(gens(stab_onmats(M[I,I];verbose)).^p)] end, iM, iN)
    if false in p return false else return p end
  end
  l=ind(axes(M,1),axes(N,1))
  if l==false return nothing end
  I=Int[]
  J=Int[]
  g=PermGroup()
  for r in l
    append!(I, r[1])
    append!(J, r[2])
    s=length(r[1])
    g=Group(vcat(gens(g), gens(r[3])))
    p=mappingPerm(I, eachindex(I))
    h=Group(gens(g).^p)
    if M[I,I]!=N[J,J]
      if verbose print("I==$(length(I)) stab==$(length(g)) ") end
      e = transporting_elt(h, M[I,I], N[J,J], onmats)
      if isnothing(e) return nothing else I=permute(I,e) end
    end
    h=centralizer(h, M[I,I], onmats)
    g=Group(gens(h).^inv(p))
  end
  return mappingPerm(I,J)
end

"""
  `Perm{T}(m::AbstractMatrix,m1::AbstractMatrix;dims=1)`

returns  `p`, a `Perm{T}`, which permutes the  rows of `m1` (the columns of
`m1`  if `dims=2`, simultaneously the rows  and columns if `dims=(1,2)`) to
bring  them  to  those  of  `m`,  if  such  a `p` exists; returns `nothing`
otherwise.  If not given `{T}` is taken to be `{Int16}`. Needs the elements
of `m` and `m1` to be sortable.

```julia-repl
julia> Perm([0 1 0;0 0 1;1 0 0],[1 0 0;0 1 0;0 0 1];dims=1)
(1,3,2)

julia> Perm([0 1 0;0 0 1;1 0 0],[1 0 0;0 1 0;0 0 1];dims=2)
(1,2,3)

julia> m=(1:30)'.*(1:30).%15;

julia> n=permute(m,Perm(1,5,2,8,12,4,7)*Perm(3,9,11,6);dims=(1,2));

julia> Perm(m,n,dims=(1,2))
(1,5,2,8,12,4,7)(3,9,11,6)
```
"""
function Perms.Perm{T}(m::AbstractMatrix,m1::AbstractMatrix;dims=1)where T<:Integer
  if     dims==1 Perm{T}(collect(eachrow(m)),collect(eachrow(m1)))
  elseif dims==2 Perm{T}(collect(eachcol(m)),collect(eachcol(m1)))
  elseif dims==(1,2) Perm_onmats(m,m1)
  end
end

Perms.Perm(m::AbstractMatrix,m1::AbstractMatrix;dims=1)=Perm{Perms.Idef}(m,m1,dims=dims)

end
