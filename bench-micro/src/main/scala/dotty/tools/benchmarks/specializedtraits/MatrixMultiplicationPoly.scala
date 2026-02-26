//> using scala 3.10.0-RC1-bin-SNAPSHOT
//> using options -language:experimental.specializedTraits

package dotty.tools.benchmarks.specializedtraits

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole

// Flat row-major layout: element (i,j) of an n×n matrix is at index i*n+j
class MatManualInt(val elems: Array[Int], val n: Int):
  def apply(i: Int, j: Int): Int = elems(i * n + j)

  def matMul(other: MatManualInt, result: Array[Int]): MatManualInt =
    require(this.n == other.n)
    for
      i <- 0 until n
      k <- 0 until n
    do
      val aik = this(i, k)
      for j <- 0 until n do
        result(i * n + j) += aik * other(k, j)
    MatManualInt(result, n)

class MatManualFloat(val elems: Array[Float], val n: Int):
  def apply(i: Int, j: Int): Float = elems(i * n + j)

  def matMul(other: MatManualFloat, result: Array[Float]): MatManualFloat =
    require(this.n == other.n)
    for
      i <- 0 until n
      k <- 0 until n
    do
      val aik = this(i, k)
      for j <- 0 until n do
        result(i * n + j) += aik * other(k, j)
    MatManualFloat(result, n)

class MatManualDouble(val elems: Array[Double], val n: Int):
  def apply(i: Int, j: Int): Double = elems(i * n + j)

  def matMul(other: MatManualDouble, result: Array[Double]): MatManualDouble =
    require(this.n == other.n)
    for
      i <- 0 until n
      k <- 0 until n
    do
      val aik = this(i, k)
      for j <- 0 until n do
        result(i * n + j) += aik * other(k, j)
    MatManualDouble(result, n)

class MatGeneric[T: Numeric](val elems: Array[T], val n: Int):
  private val num = summon[Numeric[T]]

  def apply(i: Int, j: Int): T = elems(i * n + j)

  def matMul(other: MatGeneric[T], result: Array[T]): MatGeneric[T] =
    require(this.n == other.n)
    for
      i <- 0 until n
      k <- 0 until n
    do
      val aik = this(i, k)
      for j <- 0 until n do
        result(i * n + j) = num.plus(result(i * n + j), num.times(aik, other(k, j)))
    MatGeneric(result, n)

inline trait MatSpecialized[T: {Specialized, NumericSpecialized}](val elems: Array[T], val n: Int):
  private val num = summon[NumericSpecialized[T]]

  def apply(i: Int, j: Int): T = elems(i * n + j)

  def matMul(other: MatSpecialized[T], result: Array[T]): Array[T] =
    require(this.n == other.n)
    for
      i <- 0 until n
      k <- 0 until n
    do
      val aik = this(i, k)
      for j <- 0 until n do
        result(i * n + j) = num.plus(result(i * n + j), num.times(aik, other(k, j)))
    result

@State(Scope.Benchmark)
class Matrices:
  val n = 300

  val mat1Int = Array.fill(n * n)(math.round(math.random() * 100).toInt)
  val mat2Int = Array.fill(n * n)(math.round(math.random() * 100).toInt)

  val mat1Float = Array.fill(n * n)(math.random().floatValue * 100)
  val mat2Float = Array.fill(n * n)(math.random().floatValue * 100)

  val mat1Double = Array.fill(n * n)(math.random() * 100)
  val mat2Double = Array.fill(n * n)(math.random() * 100)

  val resultInt = Array.ofDim[Int](n * n)
  val resultFloat = Array.ofDim[Float](n * n)
  val resultDouble = Array.ofDim[Double](n * n)

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class MatBench:
  @Benchmark
  def manual(m: Matrices, blackHole: Blackhole) =
    val x1 = MatManualInt(m.mat1Int, m.n)
    val y1 = MatManualInt(m.mat2Int, m.n)
    blackHole.consume(x1.matMul(y1, m.resultInt))

    val x2 = MatManualFloat(m.mat1Float, m.n)
    val y2 = MatManualFloat(m.mat2Float, m.n)
    blackHole.consume(x2.matMul(y2, m.resultFloat))
  
    val x3 = MatManualDouble(m.mat1Double, m.n)
    val y3 = MatManualDouble(m.mat2Double, m.n)
    blackHole.consume(x3.matMul(y3, m.resultDouble))

  @Benchmark
  def generic(m: Matrices, blackHole: Blackhole) =
    val x1 = MatGeneric[Int](m.mat1Int, m.n)
    val y1 = MatGeneric[Int](m.mat2Int, m.n)
    blackHole.consume(x1.matMul(y1, m.resultInt))

    val x2 = MatGeneric[Float](m.mat1Float, m.n)
    val y2 = MatGeneric[Float](m.mat2Float, m.n)
    blackHole.consume(x2.matMul(y2, m.resultFloat))
  
    val x3 = MatGeneric[Double](m.mat1Double, m.n)
    val y3 = MatGeneric[Double](m.mat2Double, m.n)
    blackHole.consume(x3.matMul(y3, m.resultDouble))

  @Benchmark
  def specialized(m: Matrices, blackHole: Blackhole) =
    val x1 = new MatSpecialized[Int](m.mat1Int, m.n) {}
    val y1 = new MatSpecialized[Int](m.mat2Int, m.n) {}
    blackHole.consume(x1.matMul(y1, m.resultInt))

    val x2 = new MatSpecialized[Float](m.mat1Float, m.n) {}
    val y2 = new MatSpecialized[Float](m.mat2Float, m.n) {}
    blackHole.consume(x2.matMul(y2, m.resultFloat))

    val x3 = new MatSpecialized[Double](m.mat1Double, m.n) {}
    val y3 = new MatSpecialized[Double](m.mat2Double, m.n) {}
    blackHole.consume(x3.matMul(y3, m.resultDouble))

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

