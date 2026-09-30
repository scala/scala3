//> using scala 3.10.0-RC1-bin-SNAPSHOT
//> using options -language:experimental.specializedTraits

package dotty.tools.benchmarks.specializedtraits

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

class VecManual(elems: Array[Int]):
  private val num = summon[Numeric[Int]]

  def length = elems.length

  def apply(i: Int): Int = elems(i)

  def scalarProduct(other: VecManual): Int =
    require(this.length == other.length)
    var result = num.fromInt(0)
    for i <- 0 until length do
      result = num.plus(result, num.times(this(i), other(i)))
    result

class VecGeneric[T: Numeric](elems: Array[T]):
  private val num = summon[Numeric[T]]

  def length = elems.length

  def apply(i: Int): T = elems(i)

  def scalarProduct(other: VecGeneric[T]): T =
    require(this.length == other.length)
    var result = num.fromInt(0)
    for i <- 0 until length do
      result = num.plus(result, num.times(this(i), other(i)))
    result

inline trait VecSpecialized[T: {Specialized, Numeric2}](elems: Array[T]):
  private val num = summon[Numeric2[T]]

  def length = elems.length

  def apply(i: Int): T = elems(i)

  def scalarProduct(other: VecSpecialized[T]): T =
    require(this.length == other.length)
    var result = num.fromInt(0)
    for i <- 0 until length do
      result = num.plus(result, num.times(this(i), other(i)))
    result

@State(Scope.Benchmark)
class Arrays:
  val arr1 = Array.fill(100_000_000) {math.round(math.random().floatValue * 4)}
  val arr2 = Array.fill(100_000_000) {math.round(math.random().floatValue * 4)}
  val target = arr1.zip(arr2).map((x, y) => x * y).fold(0)(_ + _)

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class VecBench:
  @Benchmark
  def manual(arr: Arrays) =
    val x = VecManual(arr.arr1)
    val y = VecManual(arr.arr2)
    assert(x.scalarProduct(y) == arr.target)

  @Benchmark
  def generic(arr: Arrays) =
    val x = VecGeneric[Int](arr.arr1)
    val y = VecGeneric[Int](arr.arr2)
    assert(x.scalarProduct(y) == arr.target)

  @Benchmark
  def specialized(arr: Arrays) =
    val x = new VecSpecialized[Int](arr.arr1) {}
    val y = new VecSpecialized[Int](arr.arr2) {}
    assert(x.scalarProduct(y) == arr.target)

inline trait Numeric2[T: Specialized]:
  def fromInt(x: Int): T
  def plus(x: T, y: T): T
  def times(x: T, y: T): T

implicit object IntIsNumeric extends Numeric2[Int]:
  override def fromInt(x: Int): Int = x
  override def plus(x: Int, y: Int): Int = x + y
  override def times(x: Int, y: Int): Int = x * y
