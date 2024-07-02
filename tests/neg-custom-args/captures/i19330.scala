//> using options -source 3.4
// (to force sealed encapsulation checking)
import language.experimental.captureChecking

trait Logger
def usingLogger[T](op: Logger^ => T): T = ???

trait Foo:
  type T >: () => Logger^

class Bar extends Foo:
  type T = () => Logger^

def foo(x: Foo): x.T =
  val leaked = usingLogger[x.T]: l =>  // error
    val t: () => Logger^ = () => l
    t: x.T
  leaked

def test(): Unit =
  val bar = new Bar
  val bad: bar.T = foo(bar)
  val leaked: Logger^ = bad()  // leaked scoped capability!
