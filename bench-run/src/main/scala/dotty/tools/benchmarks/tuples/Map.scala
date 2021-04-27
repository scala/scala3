package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class Map {
  @Param(Array("0"))
  var size: Int = _
  var tuple: Tuple = _
  var array: Array[Object] = _

  @Setup
  def setup(): Unit = {
    tuple = Tuple()

    for (i <- 1 to size)
      tuple = "elem" *: tuple

    array = Array.fill(size)("elem")
  }

  def f: PolyFunction = new PolyFunction {
    def apply[T](x: T): T = {
      x.asInstanceOf[String].updated(0, 'a').asInstanceOf[T]
    }
  }

  type Id[X] = X

  @Benchmark
  def tupleMap(): Tuple = {
    runtime.Tuples.map[Id](tuple, [T] => (x:T) => x.asInstanceOf[String].updated(0, 'a').asInstanceOf[T])
  }

  @Benchmark
  def arrayMap(): Array[Object] = {
    array.map(x => x.asInstanceOf[String].updated(0, 'a'))
  }
}
