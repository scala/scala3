import language.experimental.captureChecking
import caps.*
trait Cap
class Inv[T] extends Capability
class Inv2[T]
class Inv3[T] extends Mutable
def test(c: Cap^): Unit =
  val t1: Inv[() ->{c} Unit] = Inv()  // error
  val t2: Inv2[() ->{c} Unit] = Inv2()  // ok
  val t3: Inv3[() ->{c} Unit] = Inv3()   // error, too