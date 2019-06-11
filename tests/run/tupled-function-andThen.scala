object Test {
  def main(args: Array[String]): Unit = {

    val f1 = (x1: Int, x2: Int) => (x1, x2, x1 + x2)
    val g1 = (x1: Int, x2: Int, x3: Int) => x1 + x2 + x3
    val h1 = f1.andThen(g1)
    println(h1(1, 2))

    val f2 = (x1: Int, x2: Int) => (1, x1, x2, x1 + x2, x1 * x2)
    val g2 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int) => (x1 + x2, x3 + x4 + x5)
    val h2 = f2.andThen(g2)
    println(h2(1, 2))

    val h3 = h2.andThen(h1)
    println(h3(1, 2))

    val f25 =
      (x0: Int, x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int, x23: Int, x24: Int) =>
        (2 * x0, 2 * x1, 2 * x2, 2 * x3, 2 * x4, 2 * x5, 2 * x6, 2 * x7, 2 * x8, 2 * x9, 2 * x10, 2 * x11, 2 * x12, 2 * x13, 2 * x14, 2 * x15, 2 * x16, 2 * x17, 2 * x18, 2 * x19, 2 * x20, 2 * x21, 2 * x22, 2 * x23, 2 * x24)
    val g25 =
      (x0: Int, x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int, x23: Int, x24: Int) =>
        (3 * x0, 3 * x1, 3 * x2, 3 * x3, 3 * x4, 3 * x5, 3 * x6, 3 * x7, 3 * x8, 3 * x9, 3 * x10, 3 * x11, 3 * x12, 3 * x13, 3 * x14, 3 * x15, 3 * x16, 3 * x17, 3 * x18, 3 * x19, 3 * x20, 3 * x21, 3 * x22, 3 * x23, 3 * x24)
    val h25 = f25.andThen(g25)
    println(h25(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25))
  }

  /** Composes two instances of TupledFunctions in a new TupledFunctions, with this function applied first
    *
    *  @tparam F a function type
    *  @tparam G a function type
    *  @tparam FArgs the tuple type with the same types as the function arguments of F
    *  @tparam GArgs the tuple type with the same types as the function arguments of G and return type of F
    *  @tparam R the return type of G
    */
  def (f: F) andThen[F, G, FArgs <: Tuple, GArgs <: Tuple, R](g: G) given (tf: TupledFunction[F, FArgs => GArgs], tg: TupledFunction[G, GArgs => R]): FArgs => R = {
    x => tg.tupled(g)(tf.tupled(f)(x))
  }

}
