import scala.annotation.alpha

trait Foo {
  @alpha("intersection")
  def *(other: Foo): Foo
}

@main def Test() = {
  val s: Foo = ???
  s * s
  s * s
  s * s
}
