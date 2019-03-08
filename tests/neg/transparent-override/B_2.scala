class B extends A {
  inline def f(x: Int): Int = inline x match { // error
    case 0 => 1
    case _ => x
  }
  def g(x: Int): Int = 1  // error
}


