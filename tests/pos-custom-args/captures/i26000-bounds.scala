class File
class Box[A](val value: A)

// Lower bounds of lambdas in argument position, which the bodies rely on
def lower(io: File^, other: File^) =
  val f = Box([C^ >: {io}] => (x: File^{C}) => (io: File^{C}))
  val _: File^{io} = f.value[{io}](File())
  val g = Box([C^, D^ >: {C}] => (x: File^{C}) => (x: File^{D}))
  val _: File^{io, other} = g.value[{io}, {io, other}](io)
  val h = Box([T >: File^{io}] => (x: T) => (io: T))
  val _: File^{io} = h.value[File^{io}](File())

// Bounds of a lambda in a block that is the result of a closure
def inClosure(x: File^) =
  val q = [C^] => (a: File^{C}) => { println(1); [D^ >: {C}] => (b: File^{D}) => (a: File^{D}) }
  val _: File^{x} = q[{x}](x)[{x}](File())

// Bounds that a closure parameter takes from a polymorphic function type
def hofSib(op: (g: [C^, D^ >: {C}] -> (y: File^{D}) -> File^{D}) => Unit): Unit = ()
def hofSibHi(op: (g: [C^, D^ <: {C}] -> (y: File^{D}) -> File^{D}) => Unit): Unit = ()
def hofOrd(op: (g: [C^, T <: File^{C}] -> (x: T) -> T) => Unit): Unit = ()
def useHof(x: File^, io: File^) =
  hofSib(g => println(g[{x}, {x, io}](io)))
  hofSibHi(g => println(g[{x}, {x}](x)))
  hofOrd(g => println(g[{io}, File^{io}](io)))
