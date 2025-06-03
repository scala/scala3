import language.experimental.captureChecking

trait Logger
def usingLogger[T](op: Logger^ => T): T = ???

def foo[T >: () => Logger^](): T =
  val leaked = usingLogger[T]: l =>  // ok
    val t: () => Logger^ = () => l
    t: T
  leaked

def test(): Unit =
  val bad: () => Logger^ = foo[() => Logger^]  // error
  val leaked: Logger^ = bad()  // leaked scoped capability!
