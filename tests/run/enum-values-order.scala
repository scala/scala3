/** immutable hashmaps (as of 2.13 collections) only store up to 4 entries in insertion order */
enum LatinAlphabet { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }

@main def Test =
  import LatinAlphabet._
  val ordered = Seq(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

  assert(ordered sameElements LatinAlphabet.values)
