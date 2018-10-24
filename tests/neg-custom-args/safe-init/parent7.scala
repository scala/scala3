trait Foo {
  @scala.annotation.cold
  def name: String

  def title: String
}

trait Bar { this: Foo =>
  val message = "hello, " + name        // ok: because `name` is marked cold

  println(title)                        // error
}