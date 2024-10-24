import language.experimental.captureChecking
trait Logger
case class Boxed[T](unbox: T)

// a horrible function, but it typechecks
def magic(l: Logger^): Logger =
  class Foo:
    def foo: Boxed[Logger^{this}] =
      // for the following line to typecheck
      //   the capture checker assumes {l} <: {this}
      Boxed[Logger^{this}](l)  // error
  val x = new Foo
  val y = x.foo.unbox  // y: Logger^{x}
  val z: Logger = y  // error
  z
