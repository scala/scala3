//> using options -experimental
import language.experimental.captureChecking
import language.experimental.separationChecking

// `Weird` is not a classifier, so `Buf` is classified as `Unscoped`, not as `Nothing`
// (which would count as `SharedCapability`), and is subject to separation checking.
def par(f: () => Unit, g: () => Unit): Unit = ()
def test(b: Buf^): Unit =
  par(() => b.put(1), () => b.put(2)) // error

