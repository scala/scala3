trait Foo {
  type Out
  def out: Out
}

object Test extends App {

  implicit def bar(implicit foo: => Foo & Singleton): foo.Out = foo.out

  var x = new Foo { type Out = String; def out = "hello" }

  implicit val y: Foo = x

  assert(bar(y) == "hello")
  assert(bar == "hello")
}
