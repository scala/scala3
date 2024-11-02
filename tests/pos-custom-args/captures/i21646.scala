import language.experimental.captureChecking
import caps.Capability

trait File extends Capability

class Resource[T <: Capability](gen: T):
  def use[U](f: T => U): U =
    f(gen) // OK, was error under unsealed

@main def run =
  val myFile: File = ???
  val r = Resource(myFile) // now ok, was error
  ()
