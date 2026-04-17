import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

@main def test =
  val convert = { [C^] => (xs: List[File^{C}]) => xs.map(_ => ()) }
  val x = File()
  val files: List[File^{x}] = List(x)
  val result = convert[{x}](files)
