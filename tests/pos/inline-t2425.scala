object Test extends App {
  rewrite def foo[T](bar: T) = {
    bar match {
      case _ => ()
    }
  }
  foo(Array(1, 2))
}
