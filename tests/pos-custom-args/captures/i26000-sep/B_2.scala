package q
import language.experimental.captureChecking
import caps.*
import p.*

def use(x: File^) =
  val r1: File^{x} = O.l.head[{x}](x)
  val r2: File^{x} = O.b.value[{x}](x)
