

abstract class Logger {
  def log1(msg: String): Unit
  inline def log2(msg: String): Unit
  inline def log3(inline msg: String): Unit
}

class Logger1 extends Logger {
  inline def log1(msg: String): Unit = ()
  inline def log2(msg: String): Unit = ()
  inline def log3(msg: String): Unit = () // error: Cannot override inline parameter with a non-inline parameter
}

class Logger2 extends Logger {
  inline def log1(inline msg: String): Unit = () // error: Cannot override non-inline parameter with an inline parameter
  inline def log2(inline msg: String): Unit = () // error: Cannot override non-inline parameter with an inline parameter
  inline def log3(inline msg: String): Unit = ()
}

trait A {
  inline def f(inline a: Int): Int
}

trait B {
  def f(a: Int): Int
}

class C extends A, B {
  inline def f(inline a: Int): Int = 3 // error: Cannot override non-inline parameter with and inline parameter
}

class D extends B, A {
  inline def f(inline a: Int): Int = 3 // error: Cannot override non-inline parameter with and inline parameter
}
