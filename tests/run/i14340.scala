class Container1 extends reflect.Selectable

class Container2(values: Map[String, Any], methods: Map[String, Seq[Int] => Any]) extends Selectable:
  def selectDynamic(name: String) = values(name)
  def applyDynamic(name: String)(arg: Int*) = methods(name)(arg)

class Foo(val value: Int) extends AnyVal
class Bar[A](val value: A) extends AnyVal

object Helpers:
  def foo = Foo(1)
  def bar = Bar(Foo(2))
  def qux1 = Bar(new Container1 { def foo = Foo(10) })
  def qux2 = Bar(new Container2(Map("foo" -> Foo(20)), Map.empty).asInstanceOf[Container2 { def foo: Foo }])

@main def Test: Unit =
  val cont1 = new Container1:
    def foo = Helpers.foo
    val bar = Helpers.bar
    def qux1 = Helpers.qux1
    def qux2 = Helpers.qux2
    def fooFromInt(i: Int) = Foo(i)

  val cont2values = Map(
    "foo" -> Helpers.foo,
    "bar" -> Helpers.bar,
    "qux1" -> Helpers.qux1,
    "qux2" -> Helpers.qux2
  )

  val cont2methods = Map(
    "fooFromInt" -> { (i: Seq[Int]) => Foo(i.head) }
  )

  val cont2 = Container2(cont2values, cont2methods).asInstanceOf[Container2 {
    def foo: Foo
    def bar: Bar[Foo]
    def qux1: Bar[Container1 { def foo: Foo }]
    def qux2: Bar[Container2 { def foo: Foo }]
    def fooFromInt(i: Int): Foo
  }]


  println(cont1.foo.value)
  println(cont2.foo.value)

  println(cont1.bar.value.value)
  println(cont2.bar.value.value)

  println(cont1.qux1.value.foo.value)
  println(cont2.qux1.value.foo.value)

  println(cont1.qux2.value.foo.value)
  println(cont2.qux2.value.foo.value)

  println(cont1.fooFromInt(100).value)
  println(cont2.fooFromInt(100).value)
