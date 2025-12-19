import language.experimental.captureChecking
import annotation.experimental
import caps.{CapSet, SharedCapability}
import caps.use
import caps.unsafe.untrackedCaptures

@experimental object Test:

  class Async extends SharedCapability

  def listener[C^](async: Async^{C}): Listener^{async} = ???

  class Listener

  class Source[X^]:
    @untrackedCaptures private var listeners: Set[Listener^{X}] = Set.empty
    def register(x: Listener^{X}): Unit =
      listeners += x

    def allListeners: Set[Listener^{X}] = listeners

  def test1[C^](async1: Async, others: List[Async^{C}]) =
    val src = Source[{async1, C}]
    val _: Set[Listener^{async1, C}] = src.allListeners
    val lst1 = listener(async1)
    val lsts = others.map(listener[C])
    val _: List[Listener^{C}] = lsts
    src.register{lst1}
    src.register(listener(async1))
    lsts.foreach(src.register(_)) // TODO: why we need to use _ explicitly here?
    others.map(listener[C]).foreach(src.register(_))
    val ls = src.allListeners
    val _: Set[Listener^{async1, C}] = ls

