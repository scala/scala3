//> using scala 3.10.0-RC1-bin-SNAPSHOT
//> using options -language:experimental.specializedTraits

package dotty.tools.benchmarks.specializedtraits

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

class VecManualInt(elems: Array[Int]):
  def length = elems.length

  def apply(i: Int): Int = elems(i)

  def scalarProduct(other: VecManualInt): Int =
    require(this.length == other.length)
    var result = 0
    for i <- 0 until length do
      result = result + (this(i) * other(i))
    result

class VecManualFloat(elems: Array[Float]):
  def length = elems.length

  def apply(i: Int): Float = elems(i)

  def scalarProduct(other: VecManualFloat): Float =
    require(this.length == other.length)
    var result = 0F
    for i <- 0 until length do
      result = result + (this(i) * other(i))
    result

class VecManualDouble(elems: Array[Double]):
  def length = elems.length

  def apply(i: Int): Double = elems(i)

  def scalarProduct(other: VecManualDouble): Double =
    require(this.length == other.length)
    var result = 0D
    for i <- 0 until length do
      result = result + (this(i) * other(i))
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

inline trait VecSpecialized[T: {Specialized, NumericSpecialized}](elems: Array[T]):
  private val num = summon[NumericSpecialized[T]]

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
  val arr1Int = Array.fill(100_000) {math.round(math.random().floatValue * 4)}
  val arr2Int = Array.fill(100_000) {math.round(math.random().floatValue * 4)}

  val arr1Float = Array.fill(100_000) {math.random().floatValue * 10}
  val arr2Float = Array.fill(100_000) {math.random().floatValue * 10}

  val arr1Double = Array.fill(100_000) {math.random() * 5}
  val arr2Double = Array.fill(100_000) {math.random() * 5}

  val targetInt = arr1Int.zip(arr2Int).map((x, y) => x * y).fold(0)(_ + _)
  val targetFloat = arr1Float.zip(arr2Float).map((x, y) => x * y).fold(0F)(_ + _)
  val targetDouble = arr1Double.zip(arr2Double).map((x, y) => x * y).fold(0D)(_ + _)
  
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class VecBench:
  @Benchmark
  def manual(arr: Arrays) =
    val x1 = VecManualInt(arr.arr1Int)
    val y1 = VecManualInt(arr.arr2Int)
    assert(x1.scalarProduct(y1) == arr.targetInt)

    val x2 = VecManualFloat(arr.arr1Float)
    val y2 = VecManualFloat(arr.arr2Float)
    assert(x2.scalarProduct(y2) == arr.targetFloat)

    val x3 = VecManualDouble(arr.arr1Double)
    val y3 = VecManualDouble(arr.arr2Double)
    assert(x3.scalarProduct(y3) == arr.targetDouble)

  @Benchmark
  def generic(arr: Arrays) =
    val x1 = VecGeneric[Int](arr.arr1Int)
    val y1 = VecGeneric[Int](arr.arr2Int)
    assert(x1.scalarProduct(y1) == arr.targetInt)

    val x2 = VecGeneric[Float](arr.arr1Float)
    val y2 = VecGeneric[Float](arr.arr2Float)
    assert(x2.scalarProduct(y2) == arr.targetFloat)

    val x3 = VecGeneric[Double](arr.arr1Double)
    val y3 = VecGeneric[Double](arr.arr2Double)
    assert(x3.scalarProduct(y3) == arr.targetDouble)

  @Benchmark
  def specialized(arr: Arrays) =
    val x1 = new VecSpecialized[Int](arr.arr1Int) {}
    val y1 = new VecSpecialized[Int](arr.arr2Int) {}
    assert(x1.scalarProduct(y1) == arr.targetInt)

    val x2 = new VecSpecialized[Float](arr.arr1Float) {}
    val y2 = new VecSpecialized[Float](arr.arr2Float) {}
    assert(x2.scalarProduct(y2) == arr.targetFloat)

    val x3 = new VecSpecialized[Double](arr.arr1Double) {}
    val y3 = new VecSpecialized[Double](arr.arr2Double) {}
    assert(x3.scalarProduct(y3) == arr.targetDouble)

inline trait NumericSpecialized[T: Specialized]:
  def fromInt(x: Int): T
  def plus(x: T, y: T): T
  def times(x: T, y: T): T

implicit object IntIsNumeric extends NumericSpecialized[Int]:
  override def fromInt(x: Int): Int = x
  override def plus(x: Int, y: Int): Int = x + y
  override def times(x: Int, y: Int): Int = x * y

implicit object FloatIsNumeric extends NumericSpecialized[Float]:
  override def fromInt(x: Int): Float = x
  override def plus(x: Float, y: Float): Float = x + y
  override def times(x: Float, y: Float): Float = x * y

implicit object DoubleIsNumeric extends NumericSpecialized[Double]:
  override def fromInt(x: Int): Double = x
  override def plus(x: Double, y: Double): Double = x + y
  override def times(x: Double, y: Double): Double = x * y

