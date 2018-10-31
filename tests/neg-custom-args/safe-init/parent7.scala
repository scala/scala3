trait Foo(x: Int) {
  @scala.annotation.icy
  def name: String = "hello"

  def title: String

  def f: Int = x
}

trait Bar { this: Foo =>
  val message = "hello, " + name        // ok: because `name` is marked icy

  println(title)                        // ok: deferred

  println(f)                            // error
}