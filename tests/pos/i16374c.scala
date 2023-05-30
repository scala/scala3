def method(using String): String = ???

inline def inlineMethod(inline op: String => Unit)(using String): Unit =
  println({ val a: Int = 1; op }.apply(method))

def test(using String) =
  inlineMethod(c => print(c))
