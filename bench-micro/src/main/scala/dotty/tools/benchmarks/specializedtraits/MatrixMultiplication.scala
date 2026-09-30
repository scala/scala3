//> using scala 3.10.0-RC1-bin-SNAPSHOT
//> using options -language:experimental.specializedTraits

package dotty.tools.benchmarks.specializedtraits

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole

// Flat row-major layout: element (i,j) of an n x n matrix is at index i*n+j
class MatManual(val elems: Array[Int], val n: Int):
  def apply(i: Int, j: Int): Int = elems(i * n + j)

  def matMul(other: MatManual, result: Array[Int]): MatManual =
    require(this.n == other.n)
    for
      i <- 0 until n
      k <- 0 until n
    do
      val aik = this(i, k)
      for j <- 0 until n do
        result(i * n + j) += aik * other(k, j)
    MatManual(result, n)

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

  val mat1 = Array.fill(n * n)(math.round(math.random() * 100).toInt)
  val mat2 = Array.fill(n * n)(math.round(math.random() * 100).toInt)

  val result = Array.ofDim[Int](n * n)

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class MatBench:
  @Benchmark
  def manual(m: Matrices, blackHole: Blackhole) =
    val x1 = MatManual(m.mat1, m.n)
    val y1 = MatManual(m.mat2, m.n)
    blackHole.consume(x1.matMul(y1, m.result))

  @Benchmark
  def generic(m: Matrices, blackHole: Blackhole) =
    val x1 = MatGeneric[Int](m.mat1, m.n)
    val y1 = MatGeneric[Int](m.mat2, m.n)
    blackHole.consume(x1.matMul(y1, m.result))

  @Benchmark
  def specialized(m: Matrices, blackHole: Blackhole) =
    val x1 = new MatSpecialized[Int](m.mat1, m.n) {}
    val y1 = new MatSpecialized[Int](m.mat2, m.n) {}
    blackHole.consume(x1.matMul(y1, m.result))

inline trait NumericSpecialized[T: Specialized]:
  def fromInt(x: Int): T
  def plus(x: T, y: T): T
  def times(x: T, y: T): T

implicit object IntIsNumeric extends NumericSpecialized[Int]:
  override def fromInt(x: Int): Int = x
  override def plus(x: Int, y: Int): Int = x + y
  override def times(x: Int, y: Int): Int = x * y
