package example

class C1(val x1: Int) extends AnyVal

class C2(val x2: Int) extends AnyVal
object C2

case class C3(x: Int)

case class C4(x: Int)
object C4

object M {
  implicit class C5(x: Int)
}

case class C6(private val x: Int)

class C7(x: Int)

class C8(private[this] val x: Int)

class C9(private[this] var x: Int)

object N {
  val anonClass = new C7(42) {
    val local = ???
  }
  val anonFun = List(1).map { i =>
    val local = 2
    local + 2
  }
}
