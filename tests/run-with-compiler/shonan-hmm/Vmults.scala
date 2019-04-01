import scala.quoted._

class Vmult[Idx, T, Unt](tring: Ring[T], vec: VecOp[Idx, Unt]) {
  private[this] val blas = new Blas1(tring, vec)
  import blas._
  def vmult(vout: OVec[Idx, T, Unt], v1: Vec[Idx, T], v2: Vec[Idx, T]): Unt = vout := v1 `*.` v2
  override def toString(): String = s"Vmult($tring, $vec)"
}

object Vmults {
  def vmult(vout: Array[Complex[Int]], v1: Array[Complex[Int]], v2: Array[Complex[Int]]): Unit = {
    val n = vout.length

    val vout_ = OVec(n, (i, v: Complex[Int]) => vout(i) = v)
    val v1_ = Vec (n, i => v1(i))
    val v2_ = Vec (n, i => v2(i))

    val V = new Vmult[Int, Complex[Int], Unit](RingComplex(RingInt), new VecSta)
    V.vmult(vout_, v1_, v2_)
  }

  def vmultCA: Expr[(Array[Complex[Int]], Array[Complex[Int]], Array[Complex[Int]]) => Unit] = '{
    (vout, v1, v2) => {
      val n = vout.length
      ${
        val vout_ = OVec[Expr[Int], Complex[Expr[Int]], Expr[Unit]]('n, (i, v) => '{vout($i) = ${Complex.of_expr_complex(v)}})
        val v1_ = Vec ('n, i => Complex.of_complex_expr('{v1($i)}))
        val v2_ = Vec ('n, i => Complex.of_complex_expr('{v2($i)}))

        val V = new Vmult[Expr[Int], Complex[Expr[Int]], Expr[Unit]](RingComplex(RingIntExpr), new VecDyn)
        V.vmult(vout_, v1_, v2_)
      }
    }
  }
}
