
import reflect.Selectable.*

val x: { def f(x: Any): String } = new { def f(x: Any) = x.toString }
val y: { def f(x: String): String } = x  // error: type mismatch (no arrow rule since `f` is not defined in parent)

@main def Test =
  println(y.f("abc"))

