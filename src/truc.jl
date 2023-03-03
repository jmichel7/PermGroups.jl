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


function arrange(n)
  p=collect(1:n)
  res=fill(0,factorial(n),n)
  k=0
  while true
    k+=1;res[k,:].=p
    i=n-1;while i>0 && p[i]>p[i+1] i-=1 end
    if iszero(i) return res end
    j=n;while p[i]>p[j] j-=1 end
    p[i],p[j]=p[j],p[i]
    i+=1
    j=n
    while i<j
      p[i],p[j]=p[j],p[i]
      i+=1;j-=1
    end
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
