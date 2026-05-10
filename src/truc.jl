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

# copied from gap3 but not much faster than current version
function RepresentativeSet(G,d,e)
  # search for an element in a coset S*s of some stabilizer S of G.
  function RepresentativeSetCoset( S, s, L )
    # if the basepoint is not in d or e, S stabilizes the set  e.
    if isempty(S) || !(S[1].b in d || S[1].b in e)
     if onsets(d,s)==e return s else return end
    end
    # if the basepoint is in d, it must be mapped to a  point  in  e.
    pnts=keys(S[1].δ).^s
    if S[1].b in d pnts=intersect(pnts,e) end
    # run through the cosets of  the  stabilizer  in  the  standard  way.
    while !isempty(pnts)
      p=pnts[1]
      ss=s
      while S[1].b^ss!=p ss=S[1].δ[p/ss]\ss end
      LL=Group(filter(l->p^l==p,gens(L)))
      elm=RepresentativeSetCoset(S[2:end],ss,LL)
      if !isnothing(elm) return elm end
      pnts=setdiff(pnts,orbit(L,p))
    end
    # there is no  element  with  the  property  in  the  coset  S*s.
  end
  if length(d)!=length(e)  return end
  S=stabchain(G,vcat(d,e))
  i=1
  while i<length(S) && (S[i].b in d || S[i].b in e) i+=1 end
  K=S[i].c
  # search through the group G=G*Id for an element mapping  d to e.
  RepresentativeSetCoset(S,one(G),K)
end

