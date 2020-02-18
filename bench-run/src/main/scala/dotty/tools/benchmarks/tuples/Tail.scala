package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

@State(Scope.Thread)
class Tail {
  @Param(Array("1"))
  var size: Int = _
  var tuple: NonEmptyTuple = _
  var array: Array[Object] = _

  @Setup
  def setup(): Unit = {
    tuple = "elem" *: ()

    for (i <- 1 until size)
      tuple = "elem" *: tuple

    array = Array.fill(size)("elem")
  }

  @Benchmark
  def tupleTail(): Unit | Product = {
    DynamicTuple.tail(tuple.asInstanceOf[Product])
  }

  @Benchmark
  def arrayTail(): Array[Object] = {
    array.tail
  }
}
