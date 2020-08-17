/** immutable hashmaps (as of 2.13 collections) only store up to 4 entries in insertion order */
enum LatinAlphabet { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }

enum LatinAlphabet2 extends java.lang.Enum[LatinAlphabet2] { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }

enum LatinAlphabet3[+T] extends java.lang.Enum[LatinAlphabet3[_]] { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }

@main def Test =

  val ordinals = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
  val labels = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

  def testLatin1() =
    import LatinAlphabet._
    val ordered = Seq(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

    assert(ordered sameElements LatinAlphabet.values)
    assert(ordinals == ordered.map(_.ordinal))
    assert(labels == ordered.map(_.productPrefix))

  def testLatin2() =
    import LatinAlphabet2._
    val ordered = Seq(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

    assert(ordered sameElements LatinAlphabet2.values)
    assert(ordinals == ordered.map(_.ordinal))
    assert(labels == ordered.map(_.name))

  def testLatin3() =
    import LatinAlphabet3._
    val ordered = Seq(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

    assert(ordered sameElements LatinAlphabet3.values)
    assert(ordinals == ordered.map(_.ordinal))
    assert(labels == ordered.map(_.name))

  testLatin1()
  testLatin2()
  testLatin3()
