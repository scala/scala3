class Foo(val value: Int) extends AnyVal
class Bar[A](val value: A) extends AnyVal

class Container1 extends reflect.Selectable

class Container2 extends Selectable:
  def selectDynamic(name: String) = Bar(name)

val cont1 = new Container1:
  def foo = new Foo(1)
  val bar = new Bar(Foo(2))
  def fooFromInt(i: Int) = new Foo(i)

val cont2 = (new Container2).asInstanceOf[Container2 { def qux: Bar[String] }]

@main def Test: Unit =
  println(cont1.foo.value)
  println(cont1.bar.value.value)
  println(cont1.fooFromInt(3).value)
  println(cont2.qux.value)