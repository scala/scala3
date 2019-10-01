package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

@State(Scope.Thread)
class ArrayOps {
  @Param(Array("0"))
  var size: Int = _
  var tuple: Tuple = _
  var array: Array[Object] = _
  var iarray: IArray[Object] = _

  @Setup
  def setup(): Unit = {
    tuple = ()

    for (i <- 1 to size)
      tuple = "elem" *: tuple

    array = Array.fill(size)("elem")
    iarray = IArray.fill(size)("elem")
  }

  @Benchmark
  def baseline(): Unit = {}

  @Benchmark
  def toArray(): Array[Object] = {
    DynamicTuple.dynamicToArray(tuple)
  }

  @Benchmark
  def toIArray(): IArray[Object] = {
    DynamicTuple.dynamicToIArray(tuple)
  }

  @Benchmark
  def fromArray(): Tuple = {
    DynamicTuple.dynamicFromArray(array)
  }

  @Benchmark
  def fromIArray(): Tuple = {
    DynamicTuple.dynamicFromIArray(iarray)
  }
}
