package dotty.tools.benchmarks.inlinetraits

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}
import scala.util.Random

import standard.IntMatrixLib.{Matrix => StdIntMatrix}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(3)
@Threads(3)
@Warmup(iterations = 3, time = 2, timeUnit = SECONDS)
@Measurement(iterations = 5, time = 5, timeUnit = SECONDS)
@OutputTimeUnit(MILLISECONDS)
@State(Scope.Benchmark)
class InlineTraitBenchmark {
  val N = 200
  def matrix(rows: Int, cols: Int): Vector[Vector[Int]] =
    Vector.tabulate(rows, cols)((_, _) => Random.nextInt())

  @Setup(Level.Iteration)
  def setup = {
    Random.setSeed(N)
  }

  @Benchmark
  def standardLib = {
    val m1 = StdIntMatrix(matrix(N, N)*)
    val m2 = StdIntMatrix(matrix(N, N)*)

    (m1 + m2) * m1 // O(N^3) loops
  }
}
