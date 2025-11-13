trait Foo extends caps.ExclusiveCapability

trait Bar extends Foo, caps.Mutable

def Test =

  def f(self: Foo^): Unit = ()

  val x: Bar^ = new Bar {}
  val g = () => f(x) // `Foo` is not Mutable so `x.rd` is charged here

  val _: () ->{x.rd} Unit = g  // surprising, but correct

