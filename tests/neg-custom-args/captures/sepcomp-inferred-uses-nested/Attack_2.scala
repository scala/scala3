import language.experimental.captureChecking
import caps.*

@main def attack(): Unit =
  val secret = Boxed("TOP-SECRET")
  val out = secret.map(user.sneaky) // error
  println(out.value)
