import scala.quoted._

// DYNAMIC

object Test {

  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    {
      val intComplex = new RingComplex(RingInt)
      import intComplex._

      println(Complex(1, 2) * Complex(4, 2))
    }

    {
      val intExprComplex = new RingComplex(RingIntExpr)
      import intExprComplex._

      val res = Complex('{1}, '{2}) * Complex('{4}, '{2})
      println(s"Complex(${run(res.re.show.toExpr)}, ${run(res.im.show.toExpr)})")
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
    Vmults.vmult(out, arr1, arr2)
    println(out.toList)

    println(run(Vmults.vmultCA.show.toExpr))

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

    println(run(MVmult.mvmult_c.show.toExpr))
    println()
    println()
    println()

    println(run(MVmult.mvmult_mc(3, 2).show.toExpr))
    println()
    println()
    println()

    println(run(MVmult.mvmult_ac(a).show.toExpr))
    println()
    println()
    println()

    println(run(MVmult.mvmult_opt(a).show.toExpr))
    println()
    println()
    println()

    println(run(MVmult.mvmult_roll(a).show.toExpr))
    println()
    println()
    println()

    println(run(MVmult.mvmult_let1(a).show.toExpr))
    println()
    println()
    println()

    println(run(MVmult.mvmult_let(a).show.toExpr))
  }
}



