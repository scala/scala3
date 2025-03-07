import language.experimental.captureChecking
import annotation.experimental
import caps.{CapSet, Capability}
import caps.use

@experimental object Test:

  class Async extends Capability

  def listener(async: Async): Listener^{async} = ???

  class Listener

  class Source[X^]:
    private var listeners: Set[Listener^{X^}] = Set.empty
    def register(x: Listener^{X^}): Unit =
      listeners += x

    def allListeners: Set[Listener^{X^}] = listeners

  def test1(async1: Async, @use others: List[Async]) =
    val src = Source[CapSet^{async1, others*}]
    val _: Set[Listener^{async1, others*}] = src.allListeners
    val lst1 = listener(async1)
    val lsts = others.map(listener)
    val _: List[Listener^{others*}] = lsts
    src.register{lst1}
    src.register(listener(async1))
    lsts.foreach(src.register(_)) // TODO: why we need to use _ explicitly here?
    others.map(listener).foreach(src.register(_))
    val ls = src.allListeners
    val _: Set[Listener^{async1, others*}] = ls


