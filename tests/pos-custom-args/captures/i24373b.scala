trait Foo extends caps.ExclusiveCapability

trait Bar extends Foo, caps.Stateful

def Test =

  def f(self: Foo^): Unit = ()

  val x: Bar^ = new Bar {}
  val g = () => f(x) // `Foo` is not Stateful so `x.rd` is charged here

  val _: () ->{x.rd} Unit = g  // surprising, but correct

