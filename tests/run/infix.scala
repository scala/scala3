import annotation.{infix, alpha}
object Test extends App {

  case class Rational(n: Int, d: Int) {
    @infix def + (that: Rational) =
      Rational(this.n * that.d + that.n * this.d, this.d * that.d)
    @infix @alpha("multiply") def * (that: Rational) =
      Rational(this.n * that.n, this.d * that.d)
  }

  val r1 = Rational(1,2)
  val r2 = Rational(2,3)
  println(r1 * r2)
  println(r1 + r2)
}