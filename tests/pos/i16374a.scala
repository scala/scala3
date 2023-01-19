def method(using String): String = ???

inline def inlineMethod(inline op: String => Unit)(using String): Unit =
  println(op(method))

def test(using String) =
  inlineMethod(c => print(c))
