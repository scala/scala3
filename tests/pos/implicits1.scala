class X(val elem: Int) {
  def foo(y: String): Int = y.length + elem
}

object X {
  implicit class BarDeco(x: X) {
    def bar: String = "!"
  }
}

object Implicits {

  implicit val impl: X = new X(0)

  implicit def conv(x: Int): X = new X(x)

  class Xdecorator(x: X) extends Object {
    def foo(cond: Boolean): Int = if (cond) x.foo("abc") else 0
  }

  implicit def XDecorator(x: X): Xdecorator = new Xdecorator(x)

  val a: Object = "abc"
  val b: Any = "abc"

  def foo(x: Int)(implicit y: X): Int = {
    println(y)
    x
  }

  val y: Int = foo(1)

  val z: X = 3

  val c: Int = y.elem

  val d: Int = z.foo("abc")

  val x: X = Byte.MinValue

  //import X.BarDeco

  println(z.bar)

  val e: Int = z.foo(true)

  // Haoyi Li's example on scala-user:

  trait Modifier

  implicit def stringNode(v: String): Modifier = new Modifier {}

  val s: Modifier = Some("rd").getOrElse("")

  val xx: Int = (1: Byte)

  // Problem with implicits over or types
  def useOrd[T: math.Ordering](xs: T*) = ()
  useOrd(Some(1), None)

}
