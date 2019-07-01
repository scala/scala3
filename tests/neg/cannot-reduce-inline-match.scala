object Test {
  inline def foo[T](x: T) =
    inline x match { // error
      case _: Int =>
    }

  foo(4)

  foo("f")

}