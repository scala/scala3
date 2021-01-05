package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Apply {
  @Param(Array("1 0"))
  var sizeAndIndex: String = _
  var tuple: NonEmptyTuple = _
  var index: Int = _

  @Setup
  def setup(): Unit = {
    val size = sizeAndIndex.split(' ')(0).toInt
    index = sizeAndIndex.split(' ')(1).toInt
    tuple = "elem" *: Tuple()

    for (i <- 1 until size)
      tuple = "elem" *: tuple
  }

  @Benchmark
  def tupleApply(): Any = {
    runtime.Tuples.apply(tuple, index)
  }

  @Benchmark
  def productElement(): Any = {
    tuple.asInstanceOf[Product].productElement(index)
  }
}
