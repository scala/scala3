import caps.{any, Mutable}
import language.experimental.captureChecking

class BadBuffer[T] extends Mutable:
  update def append(x: T): BadBuffer[T]^ = this
  def foo =
    def bar: BadBuffer[T]^ = this // error
    bar

