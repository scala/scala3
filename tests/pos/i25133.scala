class C extends Object:
  override def toString = "C"

inline def foo(inline x: Object): String = x.toString
inline def bar(inline x: Object): String = x.toString()

val c = C()
val s = foo(c)
val t = bar(c)
