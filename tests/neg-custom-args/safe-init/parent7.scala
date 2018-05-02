trait Foo {
  @scala.annotation.partial
  def name: String

  def title: String
}

trait Bar { this: Foo =>
  val message = "hello, " + name        // ok: because `name` is marked partial

  println(title)                        // error
}