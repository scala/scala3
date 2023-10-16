final class Bar
final class Inv[T]
class Foo extends scala.reflect.Selectable:
  type Boo = Bar
  final given boo1: Boo = new Bar

class Test:
  def mkInv(using bar: Bar): Inv[bar.type] = new Inv()

  def test: Unit =
    val foo1     /* : Foo { val foo2: { z1 => Foo { val inv1: Inv[(z1.boo1 : z1.Boo)] }}} */ = new Foo:
      val foo2   /* :                 { z1 => Foo { val inv1: Inv[(z1.boo1 : z1.Boo)] }}  */ = new Foo:
        val inv1 /* :                                         Inv[(   boo1 :    Boo)]     */ = mkInv /* (this.boo1) */
    val inv2 = foo1.foo2.inv1 // error
    ()
