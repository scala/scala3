def method(using String): String = ???

inline def identity[T](inline x: T): T = x

inline def inlineMethod(inline op: String => Unit)(using String): Unit =
  println(identity(op)(method))

def test(using String) =
  inlineMethod(c => print(c))
