object Test {
  def main(args: Array[String]): Unit = {

    val f0 = new Expr(() => ())
    val f1 = new Expr((i: Int) => Tuple1(i))
    val f2 = new Expr((i: Int, j: Int) => (i, i + j))
    val f3 = new Expr((i: Int, j: Int, k: Int) => (i, i + j, i + j + k))
    val f25 = new Expr(
      (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int, x23: Int, x24: Int, x25: Int) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25)
    )

    println(f0())
    println(f1(1))
    println(f2(1, 2))
    println(f3(1, 2, 3))
    println(f25(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25))

    val if1 = new Expr(given (i: Int) => Tuple1(i))
    val if2 = new Expr(given (i: Int, j: Int) => (i, i + j))
    val if3 = new Expr(given (i: Int, j: Int, k: Int) => (i, i + j, i + j + k))
    val if25 = new Expr(
      given (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int, x23: Int, x24: Int, x25: Int) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25)
    )

    println(if1.applyGiven(1))
    println(if2.applyGiven(1, 2))
    println(if3.applyGiven(1, 2, 3))
    println(if25.applyGiven(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25))

  }

  class Expr[T](val x: T)

  // Specialized only for arity 0 and one as auto tupling will not provide the disired effect
  def (e: Expr[() => R]) apply[R](): R = e.x()
  def (e: Expr[Arg => R]) apply[Arg, R](arg: Arg): R = e.x(arg)
  def (e: Expr[given Arg => R]) applyGiven[Arg, R](arg: Arg): R = e.x given arg

  // Applied to all funtions of arity 2 or more (including more than 22 parameters)
  def (e: Expr[F]) apply[F, Args <: Tuple, R](args: Args) given (tf: TupledFunction[F, Args => R]): R =
    tf.tupled(e.x)(args)
  def (e: Expr[F]) applyGiven[F, Args <: Tuple, R](args: Args) given (tf: TupledFunction[F, given Args => R]): R =
    tf.tupled(e.x) given args

}