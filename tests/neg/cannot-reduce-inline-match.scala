object Test {
  inline def foo[T](x: T) =
    inline x match {
      case _: Int =>
    }

  foo(4)

  foo("f") // error

}