trait Foo {
  def foo = 4
}
object Bar extends Foo {
  transparent def bar = super[Foo].foo // error
}
object Main {
  Bar.bar
}
