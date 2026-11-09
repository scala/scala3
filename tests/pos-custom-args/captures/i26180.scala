import scala.language.experimental.captureChecking

class IO

object Obj:
  type C^ = {caps.any}
  def foo(a: Any^{C}): Unit = ()            // was: crash in GlobalCapToLocal
  def bar(): Any^{C} = ???                  // result position
  val v: Any^{C} = ???                      // val type
  def baz(f: (Any^{C}) -> Unit): Unit = ()  // was: crash in GlobalCapToLocal.Inverse
  def qux(xs: List[Any^{C}]): Unit = ()     // boxed position

object Mixed:
  val x: IO^ = ???
  type C^ = {caps.any, x}
  def foo(a: Any^{C}): Unit = ()

class Cls:
  type C^ = {caps.any}
  def foo(a: Any^{C}): Unit = ()
  def bar(a: Any^{this.C}): Unit = ()

def test(io: IO^): Unit =
  Obj.foo(io)
