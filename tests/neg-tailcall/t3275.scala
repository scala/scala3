object Test {
  @annotation.tailrec def foo() = 5 // error: not recursive
}
