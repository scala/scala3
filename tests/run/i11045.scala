abstract class Foo(x: Any)
class Boom(var x: Unit, y: Unit) extends Foo((x: Int) => x) // was error: super constructor cannot be passed a self reference
@main def Test =
  Boom((), ())

