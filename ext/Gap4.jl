module Gap4

#println("loading Gap4 extension to PermGroups")

using GAP, PermGroups

function GAP.Obj(g::Group)
  get!(g,:GAP)do
    isempty(gens(g)) ?
    GAP.Globals.Group(GAP.Obj(one(g))) :
    GAP.Globals.Group(GAP.Obj.(gens(g))...)
  end
end

# Create a GAP permutation given a vector of 16-bit integers.
# No input verification is done! Callers must ensure that the
# entries of `vec` form a permutation of the numbers in the range
# 0:length(vec)-1
function GAP.Obj(p::Perm{Int16}) # or UInt16
    vec=p.d.-1
    deg = length(vec)
    @assert deg <= 2^16
    perm = ccall((:NEW_PERM2, GAP.libgap), GapObj, (Cuint,), deg)
    addr = ccall((:ADDR_PERM2, GAP.libgap), Ptr{UInt16}, (Any,), perm)
    copyto!(unsafe_wrap(Array, addr, deg), vec)
    return perm
end

function GAP.Obj(p::Perm{Int32}) # or UInt32
    vec=p.d.-1
    deg = length(vec)
    @assert deg <= 2^32
    perm = ccall((:NEW_PERM4, GAP.libgap), GapObj, (Cuint,), deg)
    addr = ccall((:ADDR_PERM4, GAP.libgap), Ptr{UInt32}, (Any,), perm)
    copyto!(unsafe_wrap(Array, addr, deg), vec)
    return perm
end

# convert a GAP permutation into a Perm{Perms.Idef}
function Perms.Perm(perm::GapObj)
    if GAP.TNUM_OBJ(perm) == GAP.T_PERM2
        deg = ccall((:DEG_PERM2, GAP.libgap), Cuint, (Any,), perm)
        addr = ccall((:ADDR_PERM2, GAP.libgap), Ptr{UInt16}, (Any,), perm)
        vec = Vector{UInt16}(undef, deg)
        copyto!(vec, unsafe_wrap(Array, addr, deg))
        Perms.Perm_(Perms.Idef.(vec.+1))
    elseif GAP.TNUM_OBJ(perm) == GAP.T_PERM4
        deg = ccall((:DEG_PERM4, GAP.libgap), Cuint, (Any,), perm)
        addr = ccall((:ADDR_PERM4, GAP.libgap), Ptr{UInt16}, (Any,), perm)
        vec = Vector{UInt32}(undef, deg)
        copyto!(vec, unsafe_wrap(Array, addr, deg))
        Perms.Perm_(Perms.Idef.(vec.+1))
    else
        error("<perm> is not a GAP permutation")
    end
end

#@test perm=GAP.evalstr("(1,2,3)(4,5)");perm==Gap.obj(Perm(perm))

mapgap(f,gv)=map(f,GAP.gap_to_julia(gv;recursive=false))

function gapclassreps(g::PermGroup)
  gg=GAP.Obj(g)
  mapgap(Perm∘GAP.Globals.Representative,GAP.Globals.ConjugacyClasses(gg))
end

function centralizer(g::PermGroup,p::Perm)
  gg=GAP.Obj(g)
  pp=GAP.Obj(p)
  c=GAP.Globals.Centralizer(gg,pp)
  Group(Perm.(GAP.Globals.GeneratorsOfGroup(c)))
end

function intersect(g1::PermGroup,g2::PermGroup)
  gg1=GAP.Obj(g1)
  gg2=GAP.Obj(g2)
  gg=GAP.Globals.Intersection(gg1,gg2)
  Group(Perm.(GAP.Globals.GeneratorsOfGroup(gg)))
end

end
