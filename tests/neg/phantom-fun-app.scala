
class phantomFunApp {
  import Boo._ // Note: this is dangerous as it imports Boo.Any as Any

  def foo1(a: Any) = ???
  def foo2(b: BooAny) = ???

  foo1(1)
  foo1(boo[Blinky]) // error
  foo1(boo[Pinky]) // error

  foo2(boo[Blinky])
  foo2(boo[Pinky])
  foo2(1) // error
  foo2("abc") // error
  foo2(???) // error
}

object Boo extends Phantom {
  type BooAny = this.Any
  type Blinky <: BooAny
  type Pinky <: Blinky
  def boo[B <: BooAny]: B = assume
}
