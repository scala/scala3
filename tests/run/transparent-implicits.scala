case class X()
case class Y()

object impl {
  implicit val y: Y = new Y()
}

object inlines {
  import impl._

  class C {
    implicit val x: X = new X()

    transparent
    def f(): (X, Y) =
      (implicitly[X], implicitly[Y])
  }
}

object Test {
  def main(args: Array[String]) = {
    val c = new inlines.C
    val xy = c.f()
    println(xy)
  }
}
