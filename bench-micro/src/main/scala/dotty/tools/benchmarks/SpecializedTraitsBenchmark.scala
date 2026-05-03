// Run with: scala-cli --power --jmh bench-micro/src/main/scala/dotty/tools/benchmarks/SpecializedTraitsBenchmark.scala
// May have to run it again / delete .scala-build and rerun if you get a class not found
// error from scala-cli first time - the --jmh flag is still experimental.
// Don't forget to publish the compiler first and check that the version below corresponds to the generated version,
// as well as to kill the bloop server if you are republishing the same version as before.
// scala-cli --power bloop exit

//> using scala 3.8.4-RC1-bin-SNAPSHOT-nonbootstrapped
//> using options -language:experimental.specializedTraits

package dotty.tools.benchmarks

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
  var arr1 = Array.fill(100_000_000) {math.round(math.random().floatValue * 4)}
  var arr2 = Array.fill(100_000_000) {math.round(math.random().floatValue * 4)}
  val target = arr1.zip(arr2).map((x, y) => x * y).fold(0)(_ + _)
  
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 100, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 15, time = 100, timeUnit = TimeUnit.MILLISECONDS)
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

// You can really see the impact of Specialized on the interface usage here
// Remove Specialized and see that the generated code gets much more boxing and unboxing
// which slows it down substantially. 
inline trait Numeric2[T: Specialized]:
  def fromInt(x: Int): T
  def plus(x: T, y: T): T
  def times(x: T, y: T): T

implicit object IntIsIntegral extends Numeric2[Int]:
  override def fromInt(x: Int): Int = x
  override def plus(x: Int, y: Int): Int = x + y
  override def times(x: Int, y: Int): Int = x * y
