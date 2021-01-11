abstract class Foo(x: Any)
class Boom(var x: Unit, y: Unit) extends Foo((x: Int) => x)