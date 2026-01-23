import caps.*

class IO extends SharedCapability

val out: IO^ = IO()

object Test:
  val inc = () =>
    println(out)
    0
  val _ = List.fill(10)(inc())
