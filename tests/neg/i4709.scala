class Context
class ContextBase { def settings = 1 }

class Test {
  implicit def toBase(ctx: Context): ContextBase = ???

  def test(ctx0: Context) = {
    implicit val ctx = { ctx0.settings; ??? } // error
  }
  def f: Unit = { implicitly[Int]; implicit val i = implicitly[Int] } // error
}
