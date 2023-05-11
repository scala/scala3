trait Sel extends Selectable

extension (s: Sel)
  def selectDynamic(name: String) = ???
  def applyDynamic(name: String)(x: Int) = ???
  def applyDynamic(name: String)() = ???

val sel = (new Sel {}).asInstanceOf[Sel{ def foo: String; def bar(x: Int): Int; def baz(): Int }]
val foo = sel.selectDynamic("foo")
val foo2 = sel.foo
val foo3 = sel.bar(2)
val foo4 = sel.baz()


