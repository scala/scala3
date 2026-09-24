// Performance guard for the match-type reduction cache (see `MatchType.reduced`
// in `Types.scala`), driven by `MatchTypeReduceBenchmark`.
//
// `MatchType.reduced` caches its result and revalidates the cache on every call
// by walking a "reduction context footprint": the set of types whose meaning
// can change between typer states. The per-call cost is proportional to the
// size of that footprint, so the footprint must stay as small as correctness
// allows — in particular it must not grow with the size of a case body.
//
// Two ingredients are needed to put that cost on the measured path:
//   1. The match type must stay stuck (it cannot reduce at definition time),
//      otherwise the typer eliminates the `MatchType` node before any caching
//      logic runs. Here the scrutinee is an abstract type member of an
//      enclosing class, so the match is stuck while the class is
//      uninstantiated.
//   2. The case body must mention many types that a naive footprint traversal
//      would record — here all 16 class type parameters of `Outer`, nested
//      four levels deep so a deep traversal of the body walks 64 positions.
//      None of them can affect the cached reduction, so none of them belong
//      in the footprint; if any do, this benchmark regresses by roughly an
//      order of magnitude.

class Outer[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]:
  type Stuck
  type MT = Stuck match
    case Int =>
      (((A, B, C, D), (E, F, G, H), (I, J, K, L), (M, N, O, P)),
       ((P, O, N, M), (L, K, J, I), (H, G, F, E), (D, C, B, A)),
       ((A, P, B, O), (C, N, D, M), (E, L, F, K), (G, J, H, I)),
       ((I, H, J, G), (K, F, L, E), (M, D, N, C), (O, B, P, A)))
  val matchTypeHeavyBody: MT = ???
