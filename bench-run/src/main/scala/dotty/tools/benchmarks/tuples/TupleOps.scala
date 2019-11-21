package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
class TupleOps {
  var tuple1: Tuple = _
  var tuple2: Tuple = _

  @Setup
  def setup(): Unit = {
    tuple1 = ()
    for (i <- 1 until 15)
      tuple1 = s"elem$i" *: tuple1

    tuple2 = ()
    for (i <- 1 until 10)
      tuple2 = s"elem$i" *: tuple2
  }

  def tupleFlatMap(tuple: Tuple, f: [A] => A => Tuple): Tuple = {
    def tailRecFlatMap(t: Tuple, acc: Tuple): Tuple = t match {
      case () => acc
      case x *: rest => tailRecFlatMap(rest, acc ++ f(x))
    }
    tailRecFlatMap(tuple, ())
  }

  def tupleReverse(tuple: Tuple): Tuple = {
    def tailRecReverse(t: Tuple, acc: Tuple): Tuple = t match {
      case () => acc
      case x *: rest => tailRecReverse(rest, x *: acc)
    }
    tailRecReverse(tuple, ())
  }

  @Benchmark
  def reverse(): Tuple = {
    tupleReverse(tuple1)
  }

  @Benchmark
  def flatMap(): Tuple = {
    tupleFlatMap(tuple2, [A] => (x: A) => (x, x))
  }
}
