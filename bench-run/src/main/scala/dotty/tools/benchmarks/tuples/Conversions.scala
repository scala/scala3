package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

@State(Scope.Thread)
class Conversions {
  @Param(Array("1"))
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
  def tupleToArray(): Array[Object] = {
    DynamicTuple.toArray(tuple)
  }

  @Benchmark
  def tupleToIArray(): IArray[Object] = {
    DynamicTuple.toIArray(tuple)
  }

  @Benchmark
  def tupleFromArray(): Tuple = {
    DynamicTuple.fromArray(array)
  }

  @Benchmark
  def tupleFromIArray(): Tuple = {
    DynamicTuple.fromIArray(iarray)
  }

  @Benchmark
  def productToArray(): Array[Object] = {
    DynamicTuple.productToArray(tuple.asInstanceOf[Product])
  }

  @Benchmark
  def cloneArray(): Array[Object] = {
    array.clone()
  }
}
