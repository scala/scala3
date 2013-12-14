class X(val elem: Int) extends Object {
  def foo(y: String): Int = y.length + elem
}

object Functions {

  val x = new X(2)
  val xe = x.elem
  val xf: String => Int = x.foo(_: String)
  val xf2: String => Int = x.foo(_)
  val x2: String => Int = x.foo
  val x3 = x.foo _
 /*
  abstract class Spore[T, U] {
    def run(x: T): U
  }
  
  trait Spore2[T, U] { self: Spore2[T, U] =>
    def run(x: T): U
  }
  
  val x: String => String = {
    case "abc" => ""
    case x => x
  }
  val y: PartialFunction[String, String] = x => x match {
    case "abc" => ""
    case _ => x
  }
  val z: Spore[String, String] = x => x + x
  val z2: Spore2[String, String] = x => x + x
  */
}
