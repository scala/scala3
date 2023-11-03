import language.experimental.captureChecking

class C[T]:
  object inner:
    val x: T^{cap[C]} = ???  // error

class C1[T]:
  private object inner:
    val x: T^{cap[C1]} = ???  // ok
