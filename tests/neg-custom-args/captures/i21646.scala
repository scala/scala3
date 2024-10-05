import language.experimental.captureChecking
import caps.Capability

trait File extends Capability

class Resource[T <: Capability](gen: T):
  def use[U](f: T => U): U =
    f(gen) // error

@main def run =
  val myFile: File = ???
  val r = Resource(myFile) // error
  ()
