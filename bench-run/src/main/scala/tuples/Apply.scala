package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

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
    tuple = "elem" *: ()

    for (i <- 1 until size)
      tuple = "elem" *: tuple
  }

  @Benchmark
  def baseline(): Unit = {}

  @Benchmark
  def normal(): Any = {
    tuple(index)
  }

  @Benchmark
  def inlined(): Any = {
    DynamicTuple.dynamicApply(tuple, index)
  }
}
