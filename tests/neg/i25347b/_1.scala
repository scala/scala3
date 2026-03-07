trait T {
  object i {
    class C
    private object C {
      import scala.language.implicitConversions
      given c2i: Conversion[C, Int] = _ => 42
    }
  }
}

object O {
  def foo(x: Int): String = "hi"
}
