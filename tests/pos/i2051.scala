class A[T](val x:T)
class B[T](override val x:T) extends A[T](x)

class C[T](val x:T, val y: Int, val z: Boolean)
class D[T](override val x:T, y: Int, z: Boolean) extends C[T](x, y, z)

trait X(val x: Int, y: Int, z: Int)
trait Y(override val x: Int, y: Int, z: Int) extends X
class Z(override val x: Int, y: Int, z: Int) extends Y(x, y, z) with X(x, y, z)
