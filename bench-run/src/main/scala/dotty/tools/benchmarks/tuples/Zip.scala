package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.runtime.DynamicTuple

@State(Scope.Thread)
class Zip {
  @Param(Array("0"))
  var size: Int = _
  var tuple1: Tuple = _
  var tuple2: Tuple = _
  var array1: Array[Object] = _
  var array2: Array[Object] = _

  @Setup
  def setup(): Unit = {
    tuple1 = ()
    tuple2 = ()

    for (i <- 1 to size) {
      tuple1 = "el" *: tuple1
      tuple2 = "em" *: tuple2
    }

    array1 = Array.fill(size)("el")
    array2 = Array.fill(size)("em")
  }

  @Benchmark
  def tupleZip(): Tuple = {
    DynamicTuple.dynamicZip(tuple1, tuple2)
  }

  @Benchmark
  def arrayZip(): Array[(Object, Object)] = {
    array1.zip(array2)
  }
}
