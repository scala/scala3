import language.experimental.captureChecking
import scala.caps.SharedCapability
class File() {
  def write(s: String): Unit = ???
}
def usingFile[A](f: File^ => A): A = ???

case class Foo(f: [A] => () => Unit)
def leak = {
  usingFile(f =>    // error
    Foo([A] => () => f.write(""))
  ).f[Nothing]()
}