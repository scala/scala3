class Foo {
  opaque type Num = Int

  val z = Test.id(this)(1)
}
object Test {
  def id(f: Foo)(x: f.Num): f.Num = x
}