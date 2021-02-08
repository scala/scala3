/** immutable hashmaps (as of 2.13 collections) only store up to 4 entries in insertion order */
enum LatinAlphabet { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }

enum LatinAlphabet2 extends java.lang.Enum[LatinAlphabet2] { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }

enum LatinAlphabet3[+T] extends java.lang.Enum[LatinAlphabet3[_]] { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }

object Color:
  trait Pretty
enum Color extends java.lang.Enum[Color]:
  case Red, Green, Blue
  case Aqua extends Color with Color.Pretty
  case Grey, Black, White
  case Emerald extends Color with Color.Pretty
  case Brown

@main def Test =


  def testLatin() =

    val ordinals = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
    val labels   = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

    def testLatin1() =
      import LatinAlphabet.*
      val ordered = Seq(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

      assert(ordered sameElements LatinAlphabet.values)
      assert(ordinals == ordered.map(_.ordinal))
      assert(labels == ordered.map(_.productPrefix))

    def testLatin2() =
      import LatinAlphabet2.*
      val ordered = Seq(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

      assert(ordered sameElements LatinAlphabet2.values)
      assert(ordinals == ordered.map(_.ordinal))
      assert(labels == ordered.map(_.name))

    def testLatin3() =
      import LatinAlphabet3.*
      val ordered = Seq(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

      assert(ordered sameElements LatinAlphabet3.values)
      assert(ordinals == ordered.map(_.ordinal))
      assert(labels == ordered.map(_.name))

    testLatin1()
    testLatin2()
    testLatin3()

  end testLatin

  def testColor() =
    import Color.*
    val ordered  = Seq(Red, Green, Blue, Aqua, Grey, Black, White, Emerald, Brown)
    val ordinals = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8)
    val labels   = Seq("Red", "Green", "Blue", "Aqua", "Grey", "Black", "White", "Emerald", "Brown")

    assert(ordered sameElements Color.values)
    assert(ordinals == ordered.map(_.ordinal))
    assert(labels == ordered.map(_.name))

    def isPretty(c: Color): Boolean = c match
      case _: Pretty => true
      case _         => false

    assert(!isPretty(Brown))
    assert(!isPretty(Grey))
    assert(isPretty(Aqua))
    assert(isPretty(Emerald))
    assert(Emerald.getClass != Aqua.getClass)
    assert(Aqua.getClass != Grey.getClass)
    assert(Grey.getClass == Brown.getClass)

  end testColor

  testLatin()
  testColor()
