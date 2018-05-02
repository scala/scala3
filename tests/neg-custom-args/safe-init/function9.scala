final class Foo(x: Int) {
  var title: String = _

  val f: () => Unit =
    if (x > 10)
      new Function0[Unit] {
        def apply() = title = "hello"
      }
    else
      () => println(title)    // error

  val g: () => Unit =
    if (x < 5) f else () => title = "hello"

  g()  // error

  println(title)  // error

  title = "hello"
}
