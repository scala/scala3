import language.experimental.captureChecking

final class Evaluation:
  private var events = List.empty[String]

  def start(): Iterable[Int]^{this} =
    events :+= "start"
    List(1)

  def step(): (Iterable[Int]^{this} => Iterable[Int]^{this})^{this} =
    events :+= "step"
    xs =>
      events :+= "apply"
      xs

  def check(): Unit =
    val iterator = Iterator.iterate(start())(step())
    assert(iterator.next() == List(1))
    assert(events == List("start", "step"), events)
    assert(iterator.next() == List(1))
    assert(events == List("start", "step", "apply"), events)

@main def Test = Evaluation().check()
