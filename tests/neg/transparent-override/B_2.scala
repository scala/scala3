class B extends A {
  transparent def f(x: Int): Int = x match { // error
    case 0 => 1
    case _ => x
  }
  def g(x: Int): Int = 1  // error
}


