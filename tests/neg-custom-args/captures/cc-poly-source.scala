import language.experimental.captureChecking
import annotation.experimental
import caps.{CapSet, SharedCapability}
import caps.use

@experimental object Test:

  class Label //extends SharedCapability

  class Listener

  class Source[X^]:
    @caps.unsafe.untrackedCaptures private var listeners: Set[Listener^{X}] = Set.empty
    def register(x: Listener^{X}): Unit =
      listeners += x

    def allListeners: Set[Listener^{X}] = listeners

  def test1(lbl1: Label^, lbl2: Label^) =
    val src = Source[{lbl1, lbl2}]
    def l1: Listener^{lbl1} = ???
    val l2: Listener^{lbl2} = ???
    src.register{l1}
    src.register{l2}
    val ls = src.allListeners
    val _: Set[Listener^{lbl1, lbl2}] = ls

  def test2[C^](lbls: List[Label^{C}]) =
    def makeListener(lbl: Label^): Listener^{lbl} = ???
    val listeners = lbls.map(makeListener) // error
      // we get an error here because we no longer allow contravariant cap
      // to subsume other capabilities. The problem can be solved by declaring
      // Label a SharedCapability, see cc-poly-source-capability.scala
    val src = Source[{lbls*}]
    for l <- listeners do
      src.register(l)
    val ls = src.allListeners
    val _: Set[Listener^{lbls*}] = ls


