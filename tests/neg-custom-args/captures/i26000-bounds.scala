class File
class Box[A](val value: A)

// Lower bounds of lambdas in argument position are enforced
def lower(io: File^, other: File^) =
  val f = Box([C^ >: {io}] => (x: File^{C}) => (io: File^{C}))
  f.value[{}](File()) // error
  val g = Box([C^, D^ >: {C}] => (x: File^{C}) => (x: File^{D}))
  g.value[{io}, {other}](File()) // error
  val h = Box([T >: File^{io}] => (x: T) => (io: T))
  h.value[File](File()) // error

// Bounds of a lambda in a block that is the result of a closure
def inClosure(x: File^) =
  val q = [C^] => (a: File^{C}) => { println(1); [D^ >: {C}] => (b: File^{D}) => (a: File^{D}) }
  q[{x}](x)[{}](File()) // error

// Bounds that a closure parameter takes from a polymorphic function type
def hofSib(op: (g: [C^, D^ >: {C}] -> (y: File^{D}) -> File^{D}) => Unit): Unit = ()
def hofSibHi(op: (g: [C^, D^ <: {C}] -> (y: File^{D}) -> File^{D}) => Unit): Unit = ()
def useHof(x: File^, io: File^) =
  hofSib(g => println(g[{x}, {io}](io))) // error
  hofSibHi(g => println(g[{x}, {io}](io))) // error
