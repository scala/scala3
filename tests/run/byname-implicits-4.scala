trait Foo { def next: Foo }
object Foo {
  implicit def foo(implicit rec: => Foo): Foo = new Foo { def next = rec }
}

object Test extends App {
  def implicitly2[T](implicit t: T): T = t

  val foo = implicitly2[Foo]
  assert(foo eq foo.next)
}
