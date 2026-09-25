import scala.util.Try

trait Async extends caps.Control
trait Fork:
  def await(using Async): Unit
def async[T](t: Async ?=> T): T = ???

val f: Fork = ???
val r2 = async:
  Try(f.await) // error

val r3: Try[Unit] = async:
  Try(f.await) // error
