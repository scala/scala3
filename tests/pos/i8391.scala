import scala.annotation.targetName

trait Foo {
  @targetName("intersection")
  def *(other: Foo): Foo
}

@main def Test() = {
  val s: Foo = ???
  s * s
  s * s
  s * s
}
