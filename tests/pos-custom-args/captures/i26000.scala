package i26000

import caps.fresh

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

  // A covariant container
  val list = List([C^] => (y: File^{C}) => y)
  val _: File^{x} = list.head[{x}](x)
  val _: List[File^{x}] = list.map(g => g[{x}](x))

  // Defs with inferred types, referring to their parameters
  def g = Box([C^] => (xs: List[File^{C}]) => xs)
  val _: List[File^{x}] = g.value[{x}](List(x))
  def mk(z: File^) = Box((y: File^{z}) => y)
  val _: File^{x} = mk(x).value(x)
  def mkPoly[C^](z: File^{C}) = Box((y: File^{C}) => y)
  val _: File^{x} = mkPoly[{x}](x).value(x)

  // Type argument inferred from an explicit expected type
  val explicit: Box[[C^] -> (x: File^{C}) -> File^{C}] = Box([C^] => (x: File^{C}) => x)
  val _: File^{x} = explicit.value[{x}](x)

  // Nested and interdependent capture set parameters
  val nested = Box([C^] => (xs: List[File^{C}]) => [D^] => (ys: List[File^{C, D}]) => ys)
  val _: List[File^{x, io}] = nested.value[{x}](List(x))[{io}](List(io))
  val dependent = Box([C^, D^ >: {C}] => (xs: List[File^{C}]) => (ys: List[File^{D}]) => ys)
  val _: List[File^{x, io}] = dependent.value[{x}, {x, io}](List(x))(List(io))

  // Capture sets mixing C with other capabilities
  val mixed = Box([C^] => (xs: List[File^{C, io}]) => xs)
  val _: List[File^{x, io}] = mixed.value[{x}](List(x, io))

  // A function-typed parameter
  val ho = Box([C^] => (op: File^{C} => Unit) => (y: File^{C}) => op(y))
  ho.value[{x}]((z: File^{x}) => println(z))(x)

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
  val viaIdentity = identity(viaVal)
  val _: File^{x} = viaIdentity.value[{x}](x)

  // A monomorphic closure
  val mono = Box((y: File^{x}) => y)
  val _: File^{x} = mono.value(x)

  // Parameter types with roots or with locals of the right hand side
  val withFresh = Box([C^] => (op: () -> File^{C, fresh}) => 1)
  val withFreshDep = Box([C^] => (op: (y: File^{C}) -> File^{y, fresh}) => 1)
  val withLocal =
    val loc: File^ = File()
    Box([C^] => (y: File^{C, loc}) => y)
  val localLambda =
    val loc: File^ = File()
    [C^] => (y: File^{C, loc}) => y

// A closure parameter that takes its type from a polymorphic function type
def hof(op: (g: [C^] -> (y: File^{C}) -> File^{C}) => Unit): Unit = op([C^] => (y: File^{C}) => y)
def useHof(x: File^) = hof(g => println(g[{x}](x)))

// Several sources of function types
def merged(a: File^, b: File^, c: Boolean) =
  val f = Box(if c then (y: File^{a}) => y else (y: File^{b}) => y)
  val g = Box(if c then [C^] => (y: File^{C, a}) => y else [C^] => (y: File^{C, b}) => y)
