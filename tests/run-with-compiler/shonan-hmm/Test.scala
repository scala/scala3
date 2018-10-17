import scala.quoted._

// DYNAMIC

object Test {

  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make

    {
      val intComplex = new RingComplex(new RingInt)
      import intComplex._

      println(Complex(1, 2) * Complex(4, 2))
    }

    tb.run {
      val intExprComplex = new RingComplex(new RingIntExpr)
      import intExprComplex._

      val res = Complex('{1}, '{2}) * Complex('{4}, '{2})
      println(s"Complex(${res.re.show}, ${res.im.show})")
      '()
    }

    // {
    //   val intExprComplex = implicitly[Ring[Expr[Complex[Int]]]]
    //   import intExprComplex._

    //   val res = '{Complex(1, 2)} * '{Complex(4, 2)}
    //   println(res.show)
    // }

    val arr1 = Array(Complex(1, 0), Complex(0, 4), Complex(2, 2))
    val arr2 = Array(Complex(2, 0), Complex(1, 1), Complex(1, 2))
    val out  = Array(Complex(0, 0), Complex(0, 0), Complex(0, 0))
    tb.run {
      Vmults.vmult(out, arr1, arr2)
      '(println(~out.toList.toString.toExpr))
    }

    println(tb.show(Vmults.vmultCA))

    val a = Array(
      Array( 5,  0,  0,  5,  0),
      Array( 0,  0, 10,  0,  0),
      Array( 0, 10,  0,  0,  0),
      Array( 0,  0,  2,  3,  5),
      Array( 0,  0,  3,  0,  7)
    )

    val v1 = Array(1, 2, 3, 4, 5)
    val v1out = Array(0, 0, 0, 0, 0)
    MVmult.mvmult_p(v1out, a, v1)
    println(v1out.toList)
    println()
    println()
    println()

    println(tb.show(MVmult.mvmult_c))
    println()
    println()
    println()

    println(tb.show(MVmult.mvmult_mc(3, 2)))
    println()
    println()
    println()

    println(tb.show(MVmult.mvmult_ac(a)))
    println()
    println()
    println()

    println(tb.show(MVmult.mvmult_opt(a)))
    println()
    println()
    println()

    println(tb.show(MVmult.mvmult_roll(a)))
    println()
    println()
    println()

    println(tb.show(MVmult.mvmult_let1(a)))
    println()
    println()
    println()

    println(tb.show(MVmult.mvmult_let(a)))
  }
}



