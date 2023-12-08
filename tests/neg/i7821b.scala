// scalac: -Xfatal-warnings

object Test {

  { def f(x: Int, y: Int): Int = f(x, y) } // error
  { def f(x: Int, y: Int): Int = { f(x, y) } } // error
  { def f(x: Int, y: Int): Int = f(y, x) } // error
  { def f(x: Int, y: Int): Int = f(x, x) } // error
  { def f(x: Int, y: Int): Int = f(1, 1) } // error
  { def f(x: Int, y: Int): Int = { val a = 3; f(a, 1) } } // error
  { def f(x: Int): Int = f(1) } // error

}
