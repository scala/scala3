object Test {
  def main(args: Array[String]): Unit = {

    val f0 = (args: Unit) => 0
    val g0: () => Int = f0.untupled
    println(g0())

    val f1 = (args: Tuple1[Int]) => 1
    val g1: (Int) => Int = f1.untupled
    println(g1(1))

    val f2 = (args: (Int, Int)) => 2
    val g2: (Int, Int) => Int = f2.untupled
    println(g2(1, 2))

    val f3 = (args: (Int, Int, Int)) => 3
    val g3: (Int, Int, Int) => Int = f3.untupled
    println(g3(1, 2, 3))

    val f4 = (args: (Int, Int, Int, Int)) => 4
    val g4: (Int, Int, Int, Int) => Int = f4.untupled
    println(g4(1, 2, 3, 4))

    val f5 = (args: (Int, Int, Int, Int, Int)) => 5
    val g5: (Int, Int, Int, Int, Int) => Int = f5.untupled
    println(g5(1, 2, 3, 4, 5))

    val f6 = (args: (Int, Int, Int, Int, Int, Int)) => 6
    val g6: (Int, Int, Int, Int, Int, Int) => Int = f6.untupled
    println(g6(1, 2, 3, 4, 5, 6))

    val f7 = (args: (Int, Int, Int, Int, Int, Int, Int)) => 7
    val g7: (Int, Int, Int, Int, Int, Int, Int) => Int = f7.untupled
    println(g7(1, 2, 3, 4, 5, 6, 7))

    val f8 = (args: (Int, Int, Int, Int, Int, Int, Int, Int)) => 8
    val g8: (Int, Int, Int, Int, Int, Int, Int, Int) => Int = f8.untupled
    println(g8(1, 2, 3, 4, 5, 6, 7, 8))

    val f9 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 9
    val g9: (Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f9.untupled
    println(g9(1, 2, 3, 4, 5, 6, 7, 8, 9))

    val f10 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 10
    val g10: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f10.untupled
    println(g10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

    val f11 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 11
    val g11: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f11.untupled
    println(g11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

    val f12 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 12
    val g12: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f12.untupled
    println(g12(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

    val f13 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 13
    val g13: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f13.untupled
    println(g13(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))

    val f14 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 14
    val g14: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f14.untupled
    println(g14(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))

    val f15 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 15
    val g15: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f15.untupled
    println(g15(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))

    val f16 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 16
    val g16: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f16.untupled
    println(g16(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

    val f17 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 17
    val g17: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f17.untupled
    println(g17(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))

    val f18 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 18
    val g18: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f18.untupled
    println(g18(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))

    val f19 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 19
    val g19: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f19.untupled
    println(g19(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))

    val f20 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 20
    val g20: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f20.untupled
    println(g20(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))

    val f21 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 21
    val g21: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f21.untupled
    println(g21(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))

    val f22 = (args: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => 22
    val g22: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = f22.untupled
    println(g22(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))

  }

  /** Creates an untupled version of this function: instead of single [[scala.Tuple]] argument,
    *  it accepts a N arguments.
    *
    *  This is a generalization of [[scala.Function.untupled]] that work on functions of any arity
    *
    *  @tparam F the function type
    *  @tparam Args the tuple type with the same types as the function arguments of F
    *  @tparam R the return type of F
    */
  def (f: Args => R) untupled[F, Args <: Tuple, R] given (tf: TupledFunction[F, Args => R]): F = tf.untupled(f)
}
