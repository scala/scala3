import language.experimental.captureChecking
import caps.*

@main def use(): Unit =
  val secret = Boxed("TOP-SECRET")
  val out = secret.map(purehelper.up)
  println(out.value)
