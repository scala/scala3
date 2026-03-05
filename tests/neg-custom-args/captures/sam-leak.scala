import language.experimental.captureChecking
import scala.caps.ExclusiveCapability

class File extends ExclusiveCapability {
  def write(s: String): Unit = println(s"write to file: $s")
}

def usingFile[A](name: String)(f: File => A): A = {
  val res = f(File())
  println("file closed")
  res
}

trait Leak[C^] {
  def leak(f: () ->{C} Unit): (() -> Unit)
}

type L[C^] = (() ->{C} Unit) => (() -> Unit)

def leakClosure1[C^](f: () ->{C} Unit): (() -> Unit) = {
  val l: L[C] = (f: () -> Unit) => f  // error
  l(f)
}

def leakClosure2[C^](f: () ->{C} Unit): (() -> Unit) = {
  val l: Leak[C] = (f: () -> Unit) => f  // error
  l.leak(f)
}
