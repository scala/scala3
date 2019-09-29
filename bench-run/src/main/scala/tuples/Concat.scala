package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scala.runtime.DynamicTuple

@State(Scope.Thread)
class Concat {
  @Param(Array("0 0"))
  var sizes: String = _
  var tuple1: Tuple = _
  var tuple2: Tuple = _
  var array1: Array[Object] = _
  var array2: Array[Object] = _

  def tupleOfSize(n: Int): Tuple = {
    var t: Tuple = ()
    for (i <- 1 to n)
      t = "elem" *: t
    t
  }

  @Setup
  def setup(): Unit = {
    val size1 = sizes.split(' ')(0).toInt
    val size2 = sizes.split(' ')(1).toInt
    tuple1 = tupleOfSize(size1)
    tuple1 = tupleOfSize(size2)
    array1 = Array.fill(size1)("elem")
    array2 = Array.fill(size2)("elem")
  }

  @Benchmark
  def baseline(): Unit = {}

  @Benchmark
  def normal(): Tuple = {
   tuple1 ++ tuple2
  }

  @Benchmark
  def inlined(): Tuple = {
    DynamicTuple.dynamicConcat(tuple1, tuple2)
  }

  // This part is here to try and measure the overhead of tranforming tuples to arrays, then concatenating
  // the array, and then transforming back to a tuple
  @Benchmark
  def toArray(bh: Blackhole): Unit = {
    bh.consume(DynamicTuple.dynamicToArray(tuple1))
    bh.consume(DynamicTuple.dynamicToArray(tuple2))
  }

  @Benchmark
  def arrayConcat(): Array[Object] = {
    array1 ++ array2
  }

  @Benchmark
  def fromArray(bh: Blackhole): Unit = {
    bh.consume(DynamicTuple.dynamicFromArray[Tuple](array1))
    bh.consume(DynamicTuple.dynamicFromArray[Tuple](array2))
  }
}
