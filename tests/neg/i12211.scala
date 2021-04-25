
import reflect.Selectable.*

val x: { def f(x: Any): String } = new { def f(x: Any) = x.toString }
val y: { def f(x: String): String } = x  // error: type mismatch (different signatures)

class Sink[A] { def put(x: A): Unit = {} }

@main def Test =
  println(y.f("abc"))
  val a = new Sink[String]
  val b: { def put(x: String): Unit } = a // error: type mismatch (different signatures)
  b.put("") // gave a NoSuchMethodException: Sink.put(java.lang.String)
