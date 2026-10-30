package i26000named

import language.experimental.captureChecking

class File
class Box[A](val value: A)
class Pair[A](val first: A, val second: A)
object io extends File, caps.SharedCapability
object jo extends File, caps.SharedCapability

// Closures passed as named arguments are values of the right hand side like other arguments
object Lib uses io:
  val b = Box(value = (x: File^{io}) => 1)
  val p = Pair(first = (x: File^{io}) => 1, second = (x: File^{io}) => 2)
  def use(): Int = b.value(io) + p.second(io)

  // Typer merges the types of the two lambdas, so their parameter types are inferred
  val merged = Pair((x: File^{io}) => 1, second = (x: File^{jo}) => 2)
  def local(): Unit =
    val m = Pair((x: File^{io}) => 1, second = (x: File^{jo}) => 2)
