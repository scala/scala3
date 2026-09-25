import language.experimental.captureChecking
import caps.*

@main def sepTest =
  val x: File^ = File()
  val r = fBoxed.value[{x}](x)
  val _: File^{x} = r
  val files: List[File^{io}] = List(io)
  val r2 = bounded.value[{io}](files)
  val _: List[File^{io}] = r2
