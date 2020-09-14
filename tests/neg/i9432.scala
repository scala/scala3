trait CLibrary {
  @native def foo(x: Int): Unit // error
}
