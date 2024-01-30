# work in progress
function Base.collect(S::Stabchain)
  n=maximum(s->maximum(keys(s.δ)),S)
  base=map(s->s.b,S)
  ord=invperm(vcat(base,setdiff(1:n,base)))
  k=length(S)
  c=fill(0,k)
  Λ=fill(Int[],k)
  u=[Perm() for i in 1:k]
  Δ=map(s->sort(collect(keys(s.δ)),by=i->ord[i]),S)
  l=1;c[l]=1;Λ[l]=Δ[l];u[l]=Perm()
  res=Perm{Int16}[]
  while true
    gl=prod(u[l:-1:1])
    while l<k
      l+=1
      Λ[l]=sort(Δ[l].^gl,by=i->ord[i])
      c[l]=1
      γ=preimage(Λ[l][c[l]],gl)
      u[l]=S[l].δ[γ]
      gl=u[l]*gl
    end
    push!(res,gl)
    @show gl,c
    while l>0 && c[l]==length(S[l].δ) 
      l-=1
    end
    if l==0 return res end
    c[l]+=1
    γ=Λ[l][c[l]]
    for k in 1:l-1
      γ=preimage(γ,u[k])
    end
    u[l]=S[l].δ[γ]
  end
end

function subgroupsearch(S,H,test,property)
  k=length(s)
  K=H
  f=l=k
  n=maximum(s->maximum(keys(s.δ)),S)
  base=map(s->s.b,S)
  ord=invperm(vcat(base,setdiff(1:n,base)))
  SK=stabchain(K,base)
  R=map(SK)do sl
   p=partialsort!(collected(keys(sl.δ)),1:2,by=i->ord[i])
   if p[1]==sl.b return p[2] else return p[1] end
  end
end

# transporting_elt(G::PermGroup,g,h) element of G conjugating g to h
function representativeconjugation(G,g,h,K=nothing)

  #find s'∈S*s for some stabilizer S in a stabchain of G such that g^s'==h
  function RepresentativeConjElmsCoset(S,s,L)
    # if S is trivial check whether  s  has  the  property.
    if length(S)==1
      if  g^s==h return s
      else return
      end
    end

    @show length(S)
    # let p=S.orbit[1] be  the  basepoint  of  this  stabilizer S.
    orb=collect(keys(get_stabchain(S)[1].δ))
    p=first(orb)

    # if i = img[p] is an integer it is an earlier  basepoint  that  is
    # mapped to p by g.  For it we have already fixed an image i^s;
    # so we have p^x=(i^g)^x=i^(g*x)=i^(x*h)=(i^x)^h=(i^s)^h.
    if img[p] isa Int pnts=intersect(ontuples(orb,s),[(img[p]^s)^h])

    # otherwise it is a list of possible  images  of  $p$,  i.e.,  points
    # that lie in orbits under $h$ of the same length as $p$  under  $g$.
    else pnts=intersect( ontuples(orb,s), img[p])
    end

    # run through the cosets of  the  stabilizer  in  the  standard  way.
    while !isempty(pnts)
      p=first(pnts)
      @show length(pnts),p
      ss=s
      while orb[1]^ss!=p 
       ss=get_stabchain(S)[1].δ[preimage(p,ss)]\ss
      end
      LL=Group(filter(l->p^l==p,gens(L)))
      SS=get_stabchain(S)[1].c
      SS.stabchain=get_stabchain(S)[2:end]
      elm=RepresentativeConjElmsCoset(SS, ss, LL)
      if !isnothing(elm) return elm end
      pnts=setdiff(pnts, orbit(L,p))
    end
    # there is no  element  with  the  property  in  the  coset  S.s
  end

    # handle trivial cases.
    if g==h  return one(G) end
    if g==one(G) || h==one(G) return end

    # compute the cyclestructures and compare them.
    if cycletype(g)!=cycletype(h) return end

    orbsg=orbits(g, first_moved(G):last_moved(G))
    orbsh=orbits(h, first_moved(G):last_moved(G))

    # compute a stabchain for $G$.
    # we take a base that has as often as possible $\beta_i^g = \beta_{i+1}$.
    G.stabchain=stabchain(G,vcat(orbsg...))
    base=map(x->x.b,G.stabchain)
    @show base

    # for each length make a set of points in orbits of that length under $h$
    lensh=Dict{Int,Vector{Int}}()
    for orb in orbsh 
        if !haskey(lensh,length(orb))
          lensh[length(orb)]=sort(orb)
        else
          lensh[length(orb)]=sort(vcat(lensh[length(orb)],orb))
        end
    end

    # for each basepoint
    img=Dict{Int,Any}()
    for bpt  in base
      # if this basepoint is the image of an earlier  basepoint  store  it,
      p=preimage(bpt,g)
      if p in base  && findfirst(==(p),base)<findfirst(==(bpt),base)
        img[bpt]=p
      # otherwise store the points in orbits under $h$ of the same  length.
      else
        img[bpt]=lensh[length(orbit(g,bpt))]
      end
    end

    # find a subgroup  $K$  of  $G$ which preserves the conjugation property,
    # i.e., $g^x = h$ implies $g^{x * k} = h$  for  all  $x \in G,  k \in K$.
    # any subgroup of $Centralizer( G, h )$ will do,  for example $Group(h)$,
    # we add powers of $h$ so that we know generators for stabilizers of $K$.
    if isnothing(K)
      gg=empty(gens(G))
      for gen in pushfirst!(union(map(x->gens(x.c),get_stabchain(G))...),h)
        if h^gen==h  && gen in G
          for len  in  divisors(order(gen))
            if !isone(gen^len) && !(gen^len in gg) push!(gg,gen^len) end
          end
        end
      end
      K=Group(gg)
    end

    # search through the whole group $G = G*Id$  for a  conjugating  element.
    RepresentativeConjElmsCoset( G, one(G),K)
end
