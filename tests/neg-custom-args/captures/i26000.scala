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

  // Bounds of ordinary type parameters are enforced as well
  val tbounded = Box([T <: File^{io}] => (x: T) => x)
  val r5 = tbounded.value[File^{other}](other) // error

// The bound is enforced for a closure parameter that takes its type from a polymorphic function type
def boundedHof(b: File^)(op: (g: [C^ <: {b}] -> (y: File^{C}) -> Unit) => Unit): Unit = ()
def useBoundedHof(b: File^, other: File^) =
  boundedHof(b)(g => g[{other}](other)) // error
