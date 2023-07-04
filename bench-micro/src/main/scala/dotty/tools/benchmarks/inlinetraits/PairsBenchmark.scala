package dotty.tools.benchmarks.inlinetraits

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit.SECONDS
import scala.util.Random

@Fork(10)
@Threads(3)
@Warmup(iterations = 3, time = 5, timeUnit = SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = SECONDS)
@State(Scope.Benchmark)
class PairsBenchmark {
  val numPairs: Int = 3_000_000

  def pairElems: List[(First, Second)] = List.tabulate(numPairs)(_ % 2 match {
    case 0 => (Random.nextInt(), Random.nextDouble())
    case 1 => (Random.nextInt(Char.MaxValue).asInstanceOf[Char], Random.nextInt(Short.MaxValue).asInstanceOf[Short])
  })

  @Param(Array("standard", "specialized", "inlinetrait"))
  var libType: String = _

  var pairs: List[BenchmarkPair] = _

  @Setup(Level.Trial)
  def setup = {
    Random.setSeed(numPairs)

    val pairFactory = (l: List[(First, Second)]) => l.map((_1, _2) => BenchmarkPair.ofType(libType)(_1, _2))
    pairs = pairFactory(pairElems)
  }

  @Benchmark
  def pairsBenchmark = pairs.foldLeft(0){ case (sum, pair) => pair match {
      case BenchmarkPair(i: Int, d: Double) => 7 * i + 3 * d.toInt + sum
      case BenchmarkPair(c: Char, s: Short) => 5 * c + 2 * s + sum
    }
  }
}
