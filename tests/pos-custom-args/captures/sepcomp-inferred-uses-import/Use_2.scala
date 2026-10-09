import language.experimental.captureChecking
import caps.*

@main def use(): Unit =
  val out = Boxed("hello").map(clean.pure)
  println(out.value)
