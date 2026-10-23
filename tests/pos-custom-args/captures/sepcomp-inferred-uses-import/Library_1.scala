import language.experimental.captureChecking
import caps.*

class Ex extends ExclusiveCapability

class Boxed[+T](val value: T):
  def map[B](op: T ->{any.rd} B): Boxed[B] = Boxed(op(value))

object host:
  def mkEx(): Ex^ = new Ex

object env:
  given ex: (Ex^) = host.mkEx()

object clean:
  import env.given
  def pure(s: String): String = s.toUpperCase
