class R1
class R2

class Foo { def meth(x: Int): R1 = null }
class Bar { def meth(x: Int): R2 = null }

object Impl { implicit def mkFoo(i: Int): Foo = null }
trait Trait { implicit def mkBar(i: Int): Bar = null }

import Impl.mkFoo // remove to make code compile
object Test extends Trait:
  import Impl.mkFoo
  val fails: R1 = 1.meth(1)
