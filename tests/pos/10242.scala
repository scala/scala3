
object Test {
  type Foo[A, B <: A] = A

  type Bar[X] = X match {
    case Foo[String, x] => Unit
  }
}
