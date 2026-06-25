// Regression test for PR #25987 — performance impact of deeply traversing
// match-type case bodies in the reduction-context footprint.
//
// PR review: odersky asked whether traversing case bodies (vs only patterns)
// regresses performance. WojciechMazur's OpenCB run did not surface a
// regression, but didn't have a benchmark targeted at this path either.
//
// Two ingredients are needed to exercise the cost the PR adds:
//   1. The match type stays stuck (cannot reduce at definition time),
//      otherwise the typer eliminates the `MatchType` node before any
//      caching logic runs. Here we make the scrutinee an abstract type
//      member of an enclosing class, so the match is stuck whenever the
//      class itself is uninstantiated.
//   2. The case bodies must contain types that the footprint traversal
//      records. After PR #25987 it deep-traverses case bodies, so any
//      `NamedType` whose symbol carries the `TypeParam` flag (e.g. class
//      type parameters) becomes part of the footprint. Before the PR,
//      the traversal stopped at the case node and these types were
//      ignored. Here the body references all 16 class type parameters of
//      the enclosing `Outer` class, nested 4 levels deep so the deep
//      traversal walks 64 positions.
//
// `MatchTypeReduceBenchmark` drives `MatchType.reduced` directly against
// this `MT` instance. After the PR the per-call cost of
// `changedReductionContext` grows linearly with the footprint size; with
// the parent commit the footprint was empty for this case.

class Outer[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]:
  type Stuck
  type MT = Stuck match
    case Int =>
      (((A, B, C, D), (E, F, G, H), (I, J, K, L), (M, N, O, P)),
       ((P, O, N, M), (L, K, J, I), (H, G, F, E), (D, C, B, A)),
       ((A, P, B, O), (C, N, D, M), (E, L, F, K), (G, J, H, I)),
       ((I, H, J, G), (K, F, L, E), (M, D, N, C), (O, B, P, A)))
  val matchTypeHeavyBody: MT = ???
