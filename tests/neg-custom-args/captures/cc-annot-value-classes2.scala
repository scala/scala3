import language.experimental.captureChecking
import caps.*
trait Ref extends Mutable
def kill(consume x: Ref^): Unit = ()

class C1:
  def myKill(consume x: Ref^): Unit = kill(x)  // ok

class C2(val dummy: Int) extends AnyVal:
  def myKill(consume x: Ref^): Unit = kill(x)  // ok, too

class C3:
  def myKill(x: Ref^): Unit = kill(x)  // error

class C4(val dummy: Int) extends AnyVal:
  def myKill(x: Ref^): Unit = kill(x)  // error, too
