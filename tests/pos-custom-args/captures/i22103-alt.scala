import language.experimental.captureChecking
import caps.*

trait F[+Cap^]
def expect[C^](x: F[C]): x.type = x

def test[C^](x: F[C], io: () => Unit): Unit =
  val t1 = expect[CapSet^{C^}](x)
