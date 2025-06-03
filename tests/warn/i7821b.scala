

object Test {

  { def f(x: Int, y: Int): Int = f(x, y) } // warn
  { def f(x: Int, y: Int): Int = { f(x, y) } } // warn
  { def f(x: Int, y: Int): Int = f(y, x) } // warn
  { def f(x: Int, y: Int): Int = f(x, x) } // warn
  { def f(x: Int, y: Int): Int = f(1, 1) } // warn
  { def f(x: Int, y: Int): Int = { val a = 3; f(a, 1) } } // warn
  { def f(x: Int): Int = f(1) } // warn

}