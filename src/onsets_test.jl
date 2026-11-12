# onsets_test.jl: contributed by DeepSeek
#
# A pruning test for
#
#     subgroup_search(W, g -> onsets(p,g) == p; test = onsets_test(W, p))
#
# the setwise stabiliser of a set `p` of permutations under conjugation
# (onsets(p,g) == sort([inv(g)*r*g for r in p])).  Sound for **any** PermGroup
# `W` and **any** set `p`; it knows nothing about reflections, root systems or
# Weyl groups.
#
# =====================================================================
# 1. What a test may look at
#
# subgroup_search extends a partial product u by {s*u : s ∈ G^(k)}: the elements
# agreeing with u on b_1,...,b_k.  So test(u,k) may say false only if no s*u
# satisfies the property.
#
# Hence a test must be built from an invariant of the *points*: a numbering λ of
# the points of Ω (or of pairs of points) with
#
#     λ(m^x, m2^x) == λ(m, m2)   for every x with onsets(p,x) == p.
#
# Then λ(b_i^u, b_j^u) != λ(b_i, b_j) proves that b_i^u is not b_i^x for any
# admissible x, so u has no extension.  That is the entire requirement.
#
# =====================================================================
# 2. The invariant used here
#
# ps = p, n = number of points, and
#
#     A[m,m2] = #{r ∈ ps : m^r == m2}          (an n×n matrix of small integers)
#
# A is *equivariant*: for x with x^-1 ps x = ps the map r ↦ x^-1 r x is a
# bijection of the finite set ps onto itself, so
#
#     A[m^x, m2^x] = #{r ∈ ps : (m^x)^r = m2^x}
#                  = #{r ∈ ps : (m^(x r x^-1))^x = m2^x} = #{r' ∈ ps : m^r' = m2}
#                  = A[m,m2].
#
# So every function of A is an invariant, in particular the diagonal
# A[m,m] = #{r ∈ ps : r fixes m} and "m2 is one image of m".
#
# The labels are then refined Weisfeiler-Leman style, on pairs, in a way that
# never indexes by an individual r (the colours r are only known up to the
# permutation r ↦ x^-1 r x):
#
#     λ_{t+1}(m,m2) = ( λ_t(m,m2),
#                       { λ_t(m^r, m2)     : r ∈ ps } as a multiset,
#                       { λ_t(m, m2^r)     : r ∈ ps } as a multiset,
#                       { λ_t(m^r, m2^r)   : r ∈ ps } as a multiset )
#
# Each multiset is invariant: for the first one,
# {λ_t((m^x)^r', m2^x) : r' ∈ ps} = {λ_t((m^(x^-1 r' x))^x, m2^x)} = {λ_t(m^r, m2) : r ∈ ps}
# by equivariance of λ_t.  So λ_t is equivariant for every t, and
#
#     test(u,k) = all pairs i,j ≤ k:  λ(b_i^u, b_j^u) == λ(b_i, b_j)
#
# is sound.  Pairs are what carries the pruning: with p = 6 disjoint
# transpositions in S_12 every *point* looks exactly like every other point,
# but the pair labels know the pairing, and the test above is then literally the
# centraliser test (imgs[i]^c == imgs[j]  ⟺  b_j is the pair of b_i).
#
# =====================================================================
# 3. What is not sound (each of these returns a proper subgroup, silently)
#
#   * anything computed from conjugates by the partial product itself, e.g.
#     the set {r^u : r ∈ ps} or m^(r^u).  Extensions of u are s*u, and
#     r^(s*u) = (r^s)^u with s only fixing b_1..b_k, so r^u is not preserved.
#     This is the failure mode that turns a 1152-element answer into 192.
#
#   * the conjugacy class of the point stabiliser <p>_m inside <p>, or the
#     isomorphism type of the <p>-orbit of m: x acts on <p> by x^-1 (·) x,
#     which is not inner in general (in W(E6) the triality coset acts by an
#     outer automorphism of W(D_4)).  Sizes, counts and multisets over all of
#     p are safe; "which orbit is isomorphic to which" is not.
#
# Soundness is one-sided: a coarse test only costs nodes, an unsound one
# returns the wrong group.  subgroup_search additionally records the images it
# rejected in `ruledout`, so an unsound test's damage is not recoverable.

"labels of pairs of points, equivariant under every x with onsets(ps,x) == sort(ps)"
function pair_labels(ps::AbstractVector{<:Perm}, n::Int; rounds::Int=3)
  lab = zeros(Int, n, n)
  isempty(ps) && return lab
  A = zeros(Int, n, n)
  for r in ps, m in 1:n
    A[m, m^r] += 1
  end
  d = Dict{Any,Int}()
  for m in 1:n, m2 in 1:n
    lab[m, m2] = get!(d, (A[m, m2], A[m2, m]), length(d) + 1)
  end
  for _ in 1:rounds
    d = Dict{Any,Int}()
    new = zeros(Int, n, n)
    for m in 1:n, m2 in 1:n
      s = (lab[m, m2],
           sort([lab[m^r, m2] for r in ps]),
           sort([lab[m, m2^r] for r in ps]),
           sort([lab[m^r, m2^r] for r in ps]))
      new[m, m2] = get!(d, s, length(d) + 1)
    end
    same = new == lab
    lab = new
    same && break
  end
  lab
end

"labels of points, equivariant under every x with onsets(ps,x) == sort(ps)"
function point_labels(ps::AbstractVector{<:Perm}, n::Int; rounds::Int=16)
  lab = zeros(Int, n)
  isempty(ps) && return lab
  for o in orbits(Group(collect(ps)), 1:n), m in o
    lab[m] = length(o)
  end
  phi = zeros(Int, n, n)                    # phi[m, m2] = #{r ∈ ps : m^r == m2}
  for r in ps, m in 1:n
    phi[m, m^r] += 1
  end
  for _ in 1:rounds
    sigs = [(lab[m], sort([(lab[m2], phi[m, m2], phi[m2, m])
                           for m2 in 1:n])) for m in 1:n]
    d = Dict{Any,Int}()
    new = [get!(d, s, length(d) + 1) for s in sigs]
    new == lab && break
    lab = new
  end
  lab
end

"""
    onsets_test(W, p; base=nothing, rounds=3, budget=20_000_000) -> (g, l) -> Bool

Sound pruning predicate for `subgroup_search(W, g -> onsets(p,g) == p)`, for any
`PermGroup` `W` and any set `p` of permutations.

`base` must be the base the search uses; the default is
`PermGroups.base(get_stabchain(W))`.  If the search extends the base, only the
common prefix is tested (still sound, slightly coarser).

Cost: one `O(n^2 |p|)` refinement up front, then `O(k^2)` per search node for
`k ≤ |base|`.  Above `budget` units of work the pair invariant is skipped and
the point invariant is used instead (still sound).
"""
function onsets_test(W::PermGroup, p::AbstractVector{<:Perm};
                     base=nothing, rounds::Int=3, budget::Int=20_000_000)
  ps = unique(collect(p))
  b = base === nothing ? PermGroups.base(get_stabchain(W)) : collect(base)
  L = length(b)
  isempty(ps) && return (g::Perm, l::Int) -> true
  n = Int(max(last_moved(W), maximum(last_moved, ps)))
  if n*n*length(ps) <= budget
    lab = pair_labels(ps, n; rounds)
    return function (g::Perm, l::Int)
      k = min(l, L)
      k <= 0 && return true
      imgs = [b[i]^g for i in 1:k]          # computed once: k^2 images would dominate
      for i in 1:k
        bi = b[i]
        gi = imgs[i]
        for j in 1:k
          lab[gi, imgs[j]] == lab[bi, b[j]] || return false
        end
      end
      true
    end
  end
  pl = point_labels(ps, n)
  (g::Perm, l::Int) -> all(i -> pl[b[i]^g] == pl[b[i]], 1:min(l, L))
end

function stabilizer4(W::PermGroup,p::AbstractVector{<:Perm},::typeof(onsets))
  if debug[] println("using stabilizer(PG,Vec{Perm},onsets)") end
  property(g)=onsets(p,g)==p
  test=onsets_test(W,p)
  subgroup_search(W,property;test)
end
