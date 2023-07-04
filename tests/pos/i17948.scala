object O:
  opaque type T = Int
  inline def x: Int = P.id(2)

object P:
  def id(x: O.T): O.T = x

object Test {
  def main(args: Array[String]): Unit = println(foo())

  def foo(): Int = O.x
}
