package dotty.tools.benchmarks.inlinetraits

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(3)
@Threads(3)
@Warmup(iterations = 3, time = 2, timeUnit = SECONDS)
@Measurement(iterations = 5, time = 5, timeUnit = SECONDS)
@OutputTimeUnit(MILLISECONDS)
@State(Scope.Benchmark)
class InlineTraitBenchmark {
  val n = 300

  def intMatrixElems(): Seq[Seq[Int]] =
    Seq.tabulate(n, n)((_, _) => Random.nextInt())

  @Param(Array("standard"))
  var matrixType: String = ""

  var m1: Matrix[BenchmarkMatrix] = Matrix.empty
  var m2: Matrix[BenchmarkMatrix] = Matrix.empty

  @Setup(Level.Trial)
  def setup = {
    Random.setSeed(n)
    val matrixFactory = Matrix.ofType(matrixType)
    m1 = matrixFactory(intMatrixElems())
    m2 = matrixFactory(intMatrixElems())
  }

  @Benchmark
  def matrixBenchmark = (m1 + m2) * m1 // O(n^3) loops
}
