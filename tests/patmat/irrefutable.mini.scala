sealed class M(n: Int, s: String) extends Product {
  def _1: Int = n
  def _2: String = s
  def isEmpty: Boolean = s.size > n
  def get: M = this

  def canEqual(that: Any): Boolean = true
  def productArity: Int = 2
  def productElement(n: Int): Any = ???
}

object ExM {
  def unapply(m: M): M = m

  def test(m: M) = m match {  // warning
    case ExM(s) =>
  }
}