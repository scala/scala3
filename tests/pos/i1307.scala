class Term[A]
class Number(val n: Int) extends Term[Int]
object Test {
  def f[B](t: Term[B]): B = t match {
    case y: Number => y.n
  }
}
