final class Phantom[A](val s: String) extends AnyVal {
  def compose(p: Phantom[A]): Phantom[A] = new Phantom[A](s+p.s)
}

object Test extends dotty.runtime.LegacyApp {
  val x = new Phantom[Int]("foo")
  val y = new Phantom[Int]("bar")
  val z = x compose y
  println(z.s)
}
