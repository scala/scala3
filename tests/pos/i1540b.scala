class Casey1[T](val a: T) {
  def isEmpty: Boolean = false
  def isEmpty(x: T): Boolean = ???
  def get: T = a
  def get(x: T): String = ???
}
object Casey1 { def unapply[T](a: Casey1[T]) = a }

object Test {
  def main(args: Array[String]): Unit = {
    val Casey1(x) as c = new Casey1(0)
    assert(x == c.get)
  }
}
