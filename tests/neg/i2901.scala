trait Foo {
  def foo = 4
}
object Bar extends Foo {
  inline def bar = super[Foo].foo // error
}
object Main {
  Bar.bar
}
