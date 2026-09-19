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

final class VarargEvaluation:
  private var events = List.empty[String]

  private def record(label: String, value: Int): Int =
    events :+= label
    value

  private def total(values: Int*): Int = values.sum

  def check(): Unit =
    assert(total(record("first", 1), record("second", 2)) == 3)
    assert(events == List("first", "second"), events)

@main def Test =
  Evaluation().check()
  VarargEvaluation().check()
