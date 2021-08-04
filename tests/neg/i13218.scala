class TagTest extends AnyFreeSpec:
  "a" - {
    "b" in {
      class TF[F[_]]
      meow // error
    }
  }

trait AnyFreeSpec:
  protected class Wrapper(s: String):
    def -(f: => Unit): Unit = ???
    def in(f: => Unit): Unit = ???

  implicit def wrap(s: String): Wrapper = ???
