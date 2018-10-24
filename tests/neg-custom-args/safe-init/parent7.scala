trait Foo {
  @scala.annotation.raw
  def name: String

  def title: String
}

trait Bar { this: Foo =>
  val message = "hello, " + name        // ok: because `name` is marked raw

  println(title)                        // error
}