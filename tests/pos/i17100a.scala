
import scala.language.dynamics
trait Sel extends Dynamic

extension (s: Sel)
  def selectDynamic(name: String) = ???

val sel = new Sel {}
val foo = sel.foo
val sel2 = (new Sel {}).asInstanceOf[Sel{ def foo: String }]
val foo2 = sel2.foo

