import language.experimental.captureChecking
import annotation.experimental
import caps.SharedCapability
import caps.use

@experimental object Test2:

  class List[+A]:
    def map[B](f: A => B): List[B] = ???

  class Label extends SharedCapability

  class Listener

  def test2[C^](lbls: List[Label^{C}]) =
    def makeListener(lbl: Label^{C}): Listener^{lbl} = ???
    val listeners = lbls.map(makeListener) // should work

