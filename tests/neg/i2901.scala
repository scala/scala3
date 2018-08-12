trait Foo {
  def foo = 4
}
object Bar extends Foo {
  rewrite def bar = super[Foo].foo // error
}
object Main {
  Bar.bar
}
