class X
class Y

object Test {
  type Id[T] = T

  val a: 1 = identity(1)
  val b: Id[1] = identity(1)

  val c: X | Y = identity(if (true) new X else new Y)
  val d: Id[X | Y] = identity(if (true) new X else new Y)

  def impUnion: Unit = {
    class Base
    class A extends Base
    class B extends Base
    class Inv[T]

    implicit def invBase: Inv[Base] = new Inv[Base]

    def getInv[T](x: T)(implicit inv: Inv[T]): Int = 1

    val a: Int = getInv(if (true) new A else new B)
    // If we keep unions when doing the implicit search, this would give us: "no implicit argument of type Inv[X | Y]"
    val b: Int | Any = getInv(if (true) new A else new B)
  }
}
