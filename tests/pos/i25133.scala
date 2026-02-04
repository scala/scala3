class C extends Object:
  override def toString = "C"

inline def foo(inline x: Object): String = x.toString

val c = C()
val s = foo(c)