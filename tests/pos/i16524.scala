trait Axe
trait Foo[A]
trait Bar[B] { type Res }
class Ref[C](value: C)
trait Opt[+T]

abstract class Test:
  def mash[X](ref: Ref[X], foo: Foo[X], bar: Bar[X]): bar.Res

  def mkFoo[Y]: Foo[Opt[Y & Axe]]
  def mkBar[Z]: Bar[Opt[Z & Axe]] { type Res = Z }
  val optInt: Opt[Int & Axe]

  def test = mash(new Ref(optInt), mkFoo, mkBar)
