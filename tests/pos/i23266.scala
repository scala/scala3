//> using scala 3.7.0

class Foo(v: Any) extends AnyVal:
  def bar[X](bar: X)[Y]: Any = v

@main def run: Unit =
  val f = new Foo("lol")
  println(f.bar[String]("")[Boolean])