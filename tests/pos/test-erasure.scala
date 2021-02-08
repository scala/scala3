object erasure {

  class C(x: Int) {

    def this() = this(0)
  }

  import java.lang.*

  def const[T](x: T, y: T) = x

  val x = 2
  val y = const(x, 3)

}
