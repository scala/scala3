trait Monoid[A]:
  def combine(x: A, y: A): A
  def empty: A

object Monoid:
  lazy val addInt: Monoid[Int] = new:
    val empty = 0
    def combine(x: Int, y: Int)) = x + y // error