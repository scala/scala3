import language.experimental.captureChecking

trait Logger
def usingLogger[T](op: Logger^ => T): T = ???

trait Foo:
  type T >: () => Logger^

  def foo: this.T =
    val leaked = usingLogger[T]: l =>  // error
      val t: () => Logger^ = () => l
      t: T
    leaked
