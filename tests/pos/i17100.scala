trait Sel extends Selectable

extension (s: Sel)
  def selectDynamic(name: String) = ???
  def applyDynamic(name: String)(x: Int) = ???

val sel = (new Sel {}).asInstanceOf[Sel{ def foo: String; def bar(x: Int): Int }]
val foo = sel.selectDynamic("foo")
val foo2 = sel.foo
val foo3 = sel.bar(2)
