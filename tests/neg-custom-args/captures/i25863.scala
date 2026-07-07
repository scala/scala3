import language.experimental.captureChecking
import scala.caps.*

trait File:
  def write(s: String): Unit
def usingFile[A](f: File^ => A): A = ???
def make[A](f: A -> Unit, x: A): () -> Unit = () => f(x)
def make1[A](x: A, f: A -> Unit): () -> Unit = () => f(x)

def leakTest(): Unit =
  val leaked = usingFile: file =>
    val fun: File^ -> Unit = f => f.write("")
    val error = make[File^{file}](fun, file)  // error, which is expected
    val ok = make(fun, file)   // error, as expected
    val ok1 = make1(file, fun)   // error
    ok
