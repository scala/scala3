import language.experimental.captureChecking
import language.experimental.namedTypeArguments

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  val bar = [cap C, D <: C, E <: {C,x}, F >: {x,y} <: {C,E}] => (x: Int) => 1
  def foo[cap A >: y <: x,
              B,
              C <: {x},
              D : Ctx,
              E <: C,
              F <: {C},
              G <: {x, y},
              H >: {x} <: {x,y} : Ctx]()[cap I <: {y, G, H},
                                             J <: O.z,
                                             K <: {x, O.z},
                                             L <: {x, y, O.z},
                                             M >: {x, y, O.z} <: C : Ctx,
                                             N >: x <: x,
                                             O >: O.z <: O.z] = ???
  val baz  = () => [cap C, D <: C, E <: {C,x}, F >: {x,y} <: {C,E} : Ctx] => (x: Int) => 1
  val baz2 = (i: Int) => [cap C, D <: C, E <: {C,x}, F >: {x,y} <: {C,E} : Ctx] => (x: Int) => 1
  val baz3 = (i: Int) => [cap C, D <: C, E <: {C,x}] => () => [cap F >: {x,y} <: {C,E} : Ctx] => (x: Int) => 1


trait Foo[cap U,V,W]:
  cap C = caps.cap
  cap D = {caps.cap}
  cap E >: {V,W} <: U

def test2 =
  val x: Any^ = ???
  def foo[cap A, B >: A](x: Int) = 1
  foo[cap x, x](0)
  foo[cap A = x, B = {x}](0)
  foo[cap A = {x}](0)