object Test {
  extension on[T] (t: T) {
    val v: T = ??? // error : extensions can only have defs
  }
}