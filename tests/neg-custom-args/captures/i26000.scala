import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

@main def test =
  val io: File^ = File()
  val other: File^ = File()

  // The preserved bound `[C^ <: {io}]` must still be enforced.
  val bounded = Box([C^ <: {io}] => (f: File^{C}) => f)
  val r1 = bounded.value[{other}](other) // error

  // The preserved parameter and result sets must still constrain.
  val f = Box([C^] => (x: File^{C}) => x)
  val r2: File = f.value[{io}](io) // error

  // The `fresh` upper bound of the capture set parameter in a method result
  // must reject instantiation with an external capture set.
  def g = Box([C^] => (xs: List[File^{C}]) => xs)
  val files: List[File^{io}] = List(io)
  val r3 = g.value[{io}](files) // error
