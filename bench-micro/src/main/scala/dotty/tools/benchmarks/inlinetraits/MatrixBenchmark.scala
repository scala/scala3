package dotty.tools.benchmarks.inlinetraits

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit.SECONDS
import scala.util.Random

@Fork(10)
@Threads(3)
@Warmup(iterations = 3, time = 5, timeUnit = SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = SECONDS)
@State(Scope.Benchmark)
class MatrixBenchmark {
  val n: Int = 100

  def intMatrixElems: List[List[Int]] =
    List.tabulate(n, n)((_, _) => Random.nextInt())

  @Param(Array("standard", "specialized", "inlinetrait"))
  var libType: String = _

  var m1: BenchmarkMatrix = _
  var m2: BenchmarkMatrix = _

  @Setup(Level.Trial)
  def setup = {
    Random.setSeed(n)

    val matrixFactory = BenchmarkMatrix.ofType(libType)
    m1 = matrixFactory(intMatrixElems)
    m2 = matrixFactory(intMatrixElems)
  }

  @Benchmark
  def matrixBenchmark = (m1 + m2) * m1 // O(n^3) loops
}
