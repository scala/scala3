object Test extends App {
  transparent def foo[T](bar: T) = {
    bar match {
      case _ => ()
    }
  }
  foo(Array(1, 2))
}
