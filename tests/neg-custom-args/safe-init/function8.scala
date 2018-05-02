final class Foo(x: Int) {
  var title: String = _

  val f: () => Unit =
    if (x > 10)
      new Function0[Unit] {
        def apply() = title = "hello"
      }
    else
      () => println(title)    // error

  f()     // error

  println(title)  // error

  title = "hello"
}
