import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

def test(io: File^) =
  val x: File^ = File()

  // The issue: a polymorphic lambda in argument position
  val f = Box([C^] => (f: File^{C}) => (h: File^{C}) => (g: File^{C}) => { println(h); (f, h) })
  val (r1, r2) = f.value[{x}](x)(x)(x)
  val _: File^{x} = r1
  val _: File^{x} = r2

  // A bounded capture set parameter
  val bounded = Box([C^ <: {io}] => (f: File^{C}) => f)
  val _: File^{io} = bounded.value[{io}](io)

  // Other kinds of inferred types
  val opt = Some([C^] => (xs: List[File^{C}]) => xs)
  val _: List[File^{x}] = opt.get[{x}](List(x))
  val pair = (0, [C^] => (y: File^{C}) => y)
  val _: File^{x} = pair._2[{x}](x)
  val list = List([C^] => (y: File^{C}) => y)
  val _: File^{x} = list.head[{x}](x)
  def g = Box([C^] => (xs: List[File^{C}]) => xs)
  val _: List[File] = g.value[{}](List(File()))

  // Type argument inferred from an explicit expected type
  val explicit: Box[[C^] => (x: File^{C}) -> File^{C}] = Box([C^] => (x: File^{C}) => x)
  val _: File^{x} = explicit.value[{x}](x)

  // Nested and interdependent capture set parameters
  val nested = Box([C^] => (xs: List[File^{C}]) => [D^] => (ys: List[File^{C, D}]) => ys)
  val _: List[File^{x, io}] = nested.value[{x}](List(x))[{io}](List(io))
  val dependent = Box([C^, D^ >: {C}] => (xs: List[File^{C}]) => (ys: List[File^{D}]) => ys)
  val _: List[File^{x, io}] = dependent.value[{x}, {x, io}](List(x))(List(io))

  // Capture sets mixing C with other capabilities
  val mixed = Box([C^] => (xs: List[File^{C, io}]) => xs)
  val _: List[File^{x, io}] = mixed.value[{x}](List(x, io))

  // Impure higher-order parameters
  val ho = Box([C^] => (op: File^{C} => Unit) => (y: File^{C}) => op(y))
  ho.value[{x}]((z: File^{x}) => println(z))(x)
  val hoExplicit: Box[[C^] -> (op: File^{C} => Unit) -> Unit] = Box([C^] => (op: File^{C} => Unit) => ())

  // A polymorphic lambda nested in a monomorphic one
  val inMono = Box((i: Int) => [C^] => (y: File^{C}) => y)
  val _: File^{x} = inMono.value(0)[{x}](x)

  // An explicit def type: the root in the bound of `C` stays global
  def explicitDef: Box[[C^] -> (f: File^{C}) -> Int] = Box([C^] => (f: File^{C}) => 1)
  val _ = explicitDef.value[{x}](File())

  // A polymorphic lambda value
  val p = [C^] => (y: File^{C}) => y
  val viaVal = Box(p)
  val _: File^{x} = viaVal.value[{x}](x)
  val q = p
  val _: File^{x} = q[{x}](x)
  val viaIdentity = identity(viaVal)
  val _: File^{x} = viaIdentity.value[{x}](x)

  // A monomorphic closure
  val mono = Box((y: File^{x}) => y)
  val _: File^{x} = mono.value(x)
