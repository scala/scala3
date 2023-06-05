package dotty.tools.benchmarks.inlinetraits

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(3)
@Threads(3)
@Warmup(iterations = 3, time = 3, timeUnit = SECONDS)
@Measurement(iterations = 5, time = 5, timeUnit = SECONDS)
@OutputTimeUnit(MILLISECONDS)
@State(Scope.Benchmark)
class InlineTraitBenchmark {
  // @Param(Array("100", "200", "300"))
  var matrixSize: Int = 300

  def intMatrixElems: List[List[Int]] =
    List.tabulate(matrixSize, matrixSize)((_, _) => Random.nextInt())

  @Param(Array("standard", "specialized", "inlinetrait"))
  var libType: String = _

  var m1: BenchmarkMatrix = _
  var m2: BenchmarkMatrix = _

  @Setup(Level.Trial)
  def setup = {
    Random.setSeed(matrixSize)

    val matrixFactory = BenchmarkMatrix.ofType(libType)
    m1 = matrixFactory(intMatrixElems)
    m2 = matrixFactory(intMatrixElems)
  }

  @Benchmark
  def matrixBenchmark = (m1 + m2) * m1 // O(n^3) loops
}
