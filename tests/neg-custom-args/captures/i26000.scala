import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

def test(io: File^, other: File^) =
  // The bound of the capture set parameter is enforced
  val bounded = Box([C^ <: {io}] => (f: File^{C}) => f)
  val r1 = bounded.value[{other}](other) // error

  // The parameter types are enforced
  val f = Box([C^] => (x: File^{C}) => x)
  val r2 = f.value[{io}](other) // error

  // The result captures the instantiated capture set
  val r3: File = f.value[{io}](io) // error
  val g = Box([C^] => (xs: List[File^{C}]) => [D^] => (ys: List[File^{C, D}]) => ys)
  val r4: List[File^{io}] = g.value[{io}](List(io))[{other}](List(other)) // error
