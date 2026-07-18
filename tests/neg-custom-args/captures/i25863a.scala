import language.experimental.captureChecking
import scala.caps.SharedCapability

trait File extends SharedCapability {
  def write(s: String): Unit
}
def usingFile[A](f: File => A): A = ???

def make[A](x: A, f: A => Unit): () ->{f} Unit = {
  () => f(x)
}

def leak: () -> Unit = {
  val fun: File -> Unit = { f => f.write("") }
  usingFile { file =>
    make[File^{file}](file, fun) // error
  }
}