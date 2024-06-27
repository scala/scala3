import language.experimental.captureChecking
import annotation.experimental
import caps.{CapSet, Capability}

@experimental object Test:

  class Label extends Capability

  class Listener

  class Source[X^]:
    private var listeners: Set[Listener^{X^}] = Set.empty
    def register(x: Listener^{X^}): Unit =
      listeners += x

    def allListeners: Set[Listener^{X^}] = listeners

  def test1(lbl1: Label, lbl2: Label) =
    val src = Source[CapSet^{lbl1, lbl2}]
    def l1: Listener^{lbl1} = ???
    val l2: Listener^{lbl2} = ???
    src.register{l1}
    src.register{l2}
    val ls = src.allListeners
    val _: Set[Listener^{lbl1, lbl2}] = ls

  def test2(lbls: List[Label]) =
    def makeListener(lbl: Label): Listener^{lbl} = ???
    val listeners = lbls.map(makeListener)
    val src = Source[CapSet^{lbls*}]
    for l <- listeners do
      src.register(l)
    val ls = src.allListeners
    val _: Set[Listener^{lbls*}] = ls


