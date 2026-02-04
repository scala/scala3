class CounterExample(private var count: Int):
  def bump(): Unit =
    count += 1

class Accumulator:
  private var h: Int = 0
  def f =
    val acc = Accumulator()
    acc.h = h
