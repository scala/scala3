trait Foo {
  def a = 1
  def b = 1
  def c = 1
}

class Bar(foo: Foo) {
  export foo.{a => _, b => _, _}
  val x1 = c
}
