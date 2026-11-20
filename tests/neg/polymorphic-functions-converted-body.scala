// The body of a polymorphic function literal is evaluated once and shared by all type
// applications, so it cannot be converted to its expected type by running code.
import scala.language.implicitConversions

class Foo(val f: Int => Int)
given Conversion[Int => Int, Foo] = Foo(_)

class Bar(val f: Int => Int)
inline implicit def toBar(inline f: Int => Int): Bar = Bar(f)

def test =
  val f1: [A] => Foo = [A] => (x: Int) => x + 1 // error
  val f2: [A] => Bar = [A] => (x: Int) => x + 1 // error
  val f3: [A] => Unit = [A] => (x: Int) => x + 1 // error
  val f4: [A] => String ?=> Foo = [A] => (x: Int) => x + 1 // ok, converted on each application

enum IsInt[T]:
  case Yes extends IsInt[Int]

def h[T](ev: IsInt[T]): [A <: T] => Foo = ev match
  case IsInt.Yes => [A <: Int] => (x: Int) => x + 1 // error
