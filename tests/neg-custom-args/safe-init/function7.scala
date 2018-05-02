final class Foo(x: Int) {
  var title: String = _

  val f =
    if (x > 10)
      () => title = "hello"
    else
      () => println(title)    // error

  f()     // error

  println(title)  // error

  title = "hello"
}
