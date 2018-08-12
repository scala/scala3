class B extends A {
  rewrite def f(x: Int): Int = rewrite x match { // error
    case 0 => 1
    case _ => x
  }
  def g(x: Int): Int = 1  // error
}


