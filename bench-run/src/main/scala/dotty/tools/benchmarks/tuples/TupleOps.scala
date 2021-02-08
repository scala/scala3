package dotty.tools.benchmarks.tuples

import org.openjdk.jmh.annotations._
import scala.util.Random

@State(Scope.Thread)
class TupleOps {
  var tuple1: Tuple = _
  var tuple2: Tuple = _
  var tuple3: Tuple = _

  @Setup
  def setup(): Unit = {
    tuple1 = Tuple()
    for (i <- 1 until 15)
      tuple1 = s"elem$i" *: tuple1

    tuple2 = Tuple()
    for (i <- 1 until 10)
      tuple2 = s"elem$i" *: tuple2

    val rand = new Random(12345)
    tuple3 = Tuple.fromArray(rand.shuffle(1 to 15).toArray)
  }

  def tupleFlatMap(tuple: Tuple, f: [A] => A => Tuple): Tuple = {
    def tailRecFlatMap(t: Tuple, acc: Tuple): Tuple = t match {
      case Tuple() => acc
      case x *: rest => tailRecFlatMap(rest, acc ++ f(x))
    }
    tailRecFlatMap(tuple, Tuple())
  }

  def tupleReverse(tuple: Tuple): Tuple = {
    def tailRecReverse(t: Tuple, acc: Tuple): Tuple = t match {
      case Tuple() => acc
      case x *: rest => tailRecReverse(rest, x *: acc)
    }
    tailRecReverse(tuple, Tuple())
  }

  def tupleMerge(tuple1: Tuple, tuple2: Tuple): Tuple = (tuple1, tuple2) match {
    case (_, Tuple()) => tuple1
    case (Tuple(), _) => tuple2
    case (x *: xs, y *: ys) =>
      if (x.asInstanceOf[Int] <= y.asInstanceOf[Int]) x *: tupleMerge(xs, tuple2)
      else y *: tupleMerge(tuple1, ys)
  }

  def tupleMergeSort(tuple: Tuple): Tuple =
    if (tuple.size <= 1) tuple
    else {
      val (tuple1, tuple2) = tuple.splitAt(tuple.size / 2): (Tuple, Tuple)// TODO remove ascriptions (issue with type variable constraints)
      val (sorted1, sorted2) = (tupleMergeSort(tuple1), tupleMergeSort(tuple2))
      tupleMerge(sorted1, sorted2)
    }

  @Benchmark
  def reverse(): Tuple = {
    tupleReverse(tuple1)
  }

  @Benchmark
  def flatMap(): Tuple = {
    tupleFlatMap(tuple2, [A] => (x: A) => (x, x))
  }

  @Benchmark
  def mergeSort(): Tuple = {
    tupleMergeSort(tuple3)
  }
}
