package example

class CDep[X]
class CDependenat[X /* This is a comment /* and a nested comment
*/*/](var x1: CDep[X])
class CVarArg(var x1 : Int)


class CDefaultWrapper {
  val glob = 3
  class Cdefault(val x1: Int = glob)
}
class C1(val x1: Int) extends AnyVal

class C2(val x2: Int) extends AnyVal
object C2

case class C3[Y     ](x: Int)

case class C4(x: Int)
object C4 {
  val foo: Int = 4
}

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

  val otheranon = {
    var a = 1
    a
  }

  val lastanon = {
    val a = new CVarArg(4)
    a.x1 = 8
    a.x1
  }
}