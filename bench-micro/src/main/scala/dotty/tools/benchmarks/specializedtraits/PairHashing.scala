//> using scala 3.10.0-RC1-bin-SNAPSHOT
//> using options -language:experimental.specializedTraits

package dotty.tools.benchmarks.specializedtraits

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.util.Random
import org.openjdk.jmh.infra.Blackhole

class PairGeneric[T, S](x: T, y: S):
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()

inline trait PairSpecialized[T: Specialized, S: Specialized](x: T, y: S):
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()
    
class PairStringString(x: String, y: String):
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()

@State(Scope.Benchmark)
class Pairs:
  val n = 5_000_000
  val Ms_str_str = (0 until n).map(i => PairStringString("Good Morning", i.toString())).toArray
  val As_str_str = (0 until n).map(i => PairGeneric("Good Morning", i.toString())).toArray
  val Ts_str_str = (0 until n).map(i => new PairSpecialized[String, String]("Good Morning", i.toString()) {}.asInstanceOf[Any]).toArray

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 15, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class PairHashBench:
  @Benchmark
  def manual(state: Pairs, blackHole: Blackhole) =
    var i = 0
    var res = Random.nextInt(10)
    while (i < state.n) {
      res ^= state.Ms_str_str(i).hashCode()
      i += 1
    }
    blackHole.consume(res)

  @Benchmark
  def generic(state: Pairs, blackHole: Blackhole) =
    var i = 0
    var res = Random.nextInt(10)
    while (i < state.n) {
      res ^= state.As_str_str(i).hashCode()
      i += 1
    }
    blackHole.consume(res)

  @Benchmark
  def specialized(state: Pairs, blackHole: Blackhole) =
    var i = 0
    var res = Random.nextInt(10)
    while (i < state.n) {
      res ^= state.Ts_str_str(i).asInstanceOf[PairSpecialized[String, String]].hashCode()
      i += 1
    }
    blackHole.consume(res)
