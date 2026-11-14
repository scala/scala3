import language.experimental.captureChecking
import caps.*

class Ex extends ExclusiveCapability

class Boxed[+T](val value: T):
  def map[B](op: T ->{any.rd} B): Boxed[B] = Boxed(op(value))

object host:
  def effect(msg: String)(using ex: Ex^): String = msg
  def mkEx(): Ex^ = new Ex

object outer:
  object env:
    given ex: (Ex^) = host.mkEx()

object user:
  import outer.env.given
  def sneaky(s: String): String = host.effect("effect-on:" + s)
