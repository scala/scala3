package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Conversions {
  @Param(Array("1"))
  var size: Int = _
  var tuple: Tuple = _
  var array: Array[Object] = _
  var iarray: IArray[Object] = _

  @Setup
  def setup(): Unit = {
    tuple = Tuple()

    for (i <- 1 to size)
      tuple = "elem" *: tuple

    array = Array.fill(size)("elem")
    iarray = IArray.fill(size)("elem")
  }

  @Benchmark
  def tupleToArray(): Array[Object] = {
    runtime.Tuples.toArray(tuple)
  }

  @Benchmark
  def tupleToIArray(): IArray[Object] = {
    runtime.Tuples.toIArray(tuple)
  }

  @Benchmark
  def tupleFromArray(): Tuple = {
    runtime.Tuples.fromArray(array)
  }

  @Benchmark
  def tupleFromIArray(): Tuple = {
    runtime.Tuples.fromIArray(iarray)
  }

  @Benchmark
  def productToArray(): Array[Object] = {
    runtime.Tuples.productToArray(tuple.asInstanceOf[Product])
  }

  @Benchmark
  def cloneArray(): Array[Object] = {
    array.clone()
  }
}
