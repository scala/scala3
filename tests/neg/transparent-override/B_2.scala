class B extends A {
  inline def f(x: Int): Int = inline x match { // OK
    case 0 => 1
    case _ => x
  }
  override def g(x: Int): Int = 1 // error: is not inline, cannot override an inline methiod
}


