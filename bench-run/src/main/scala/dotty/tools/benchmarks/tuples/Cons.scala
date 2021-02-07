package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Cons {
  @Param(Array("0"))
  var size: Int = _
  var tuple: Tuple = _
  var array1: Array[Object] = _
  var array2: Array[Object] = _

  @Setup
  def setup(): Unit = {
    tuple = Tuple()

    for (i <- 1 to size)
      tuple = "elem" *: tuple

    array1 = new Array[Object](size)
    array2 = new Array[Object](size + 1)
  }

  @Benchmark
  def tupleCons(): Tuple = {
    runtime.Tuples.cons("elem", tuple)
  }

  @Benchmark
  def createArray(): Array[Object] = {
    new Array[Object](size + 1)
  }

  @Benchmark
  def consArray(): Array[Object] = {
    System.arraycopy(array1, 0, array2, 1, size)
    array2
  }
}
