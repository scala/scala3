import language.experimental.modularity
import language.experimental.captureChecking
import caps.SharedCapability

class F:
  val f: AnyRef^ = ???

case class B(tracked val a: A) extends F, SharedCapability

class A extends F, SharedCapability:
  val b: B { val a: A.this.type } = B(this)

val a: A = A()

def test() =
  val x: a.b.type = a.b
  val y: x.a.type = x.a
  // x and y are two distinct singleton types with following properties:
  // x =:= a.b
  // y =:= x.a =:= a.b.a =:= a

  val cx: AnyRef^{x} = ???
  val cy: AnyRef^{y} = ???
  val caf: AnyRef^{a.f} = ???
  val cabf: AnyRef^{a.b.f} = ???
  val cxf: AnyRef^{x.f} = ???
  val cyf: AnyRef^{y.f} = ???

  // x and y subsume to each other:
  // * {x} <:< {y}: the underlying singleton of y is x.a,
  //   and the underlying singleton of x.a is a,
  //   which is a prefix for the underlying type of x (a.b),
  //   hence {x} <:< {y};
  // * {y} <:< {x}: by underlying singleton of y is x.a, whose prefix is x.
  // Hence, {x} =:= {y}.
  val x2y: AnyRef^{y} = cx
  val y2x: AnyRef^{x} = cy

  val yf2af: AnyRef^{a.f} = cyf
  val af2yf: AnyRef^{y.f} = caf
  val xf2abf: AnyRef^{a.b.f} = cxf
  val abf2xf: AnyRef^{x.f} = cabf

  // Since `x !=:= y`, {x.f} !=:= {y.f}
  val yf2xf: AnyRef^{x.f} = cyf // error
  val xf2yf: AnyRef^{y.f} = cxf // error
