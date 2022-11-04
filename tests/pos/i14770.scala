type UndefOr[A] = A | Unit

extension [A](maybe: UndefOr[A])
  def foreach(f: A => Unit): Unit =
    maybe match
      case () => ()
      case a: A => f(a)

trait Foo
trait Bar

object Baz:
  var booBap: Foo | Bar = _

def z: UndefOr[Foo | Bar] = ???

@main
def main =
  z.foreach(x => Baz.booBap = x)

def test[A](v: A | Unit): A | Unit =  v
val x1 = test(5: Int | Unit)
val x2 = test(5: String | Int | Unit)
val _: Int | Unit = x1
val _: String | Int | Unit = x2
