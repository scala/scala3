//> using scala 3.10.0-RC1-bin-SNAPSHOT
//> using options -language:experimental.specializedTraits

package dotty.tools.benchmarks.specializedtraits

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.util.Random
import org.openjdk.jmh.infra.Blackhole

case class Foo(val x: Int)
case class Bar(val y: Int)

class PairGeneric[T, S](x: T, y: S):
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()

inline trait PairSpecialized[T: Specialized, S: Specialized](x: T, y: S): 
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()
    
class PairStringFoo(x: String, y: Foo):
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()

class PairFooBar(x: Foo, y: Bar):
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()

class PairBarString(x: Bar, y: String):
  override def hashCode(): Int = x.hashCode() ^ y.hashCode()

@State(Scope.Benchmark)
class Pairs:
  val n = 5_000_000

  val Ms_str_foo = Array.fill(n) {PairStringFoo("Good Morning", Foo(Random.nextInt(10)))}
  val Ms_foo_bar = Array.fill(n) {PairFooBar(Foo(Random.nextInt(10)), Bar(Random.nextInt(10)))}
  val Ms_bar_str = Array.fill(n) {PairBarString(Bar(Random.nextInt(10)), "Hello")}

  val As_str_foo = Array.fill(n) {PairGeneric("Good Morning", Foo(Random.nextInt(10)))}
  val As_foo_bar = Array.fill(n) {PairGeneric(Foo(Random.nextInt(10)), Bar(Random.nextInt(10)))}
  val As_bar_str = Array.fill(n) {PairGeneric(Bar(Random.nextInt(10)), "Hello")}

  val Ts_str_foo = Array.fill[Any](n) {new PairSpecialized[String, Foo]("Good Morning", Foo(Random.nextInt(10))) {}}
  val Ts_foo_bar = Array.fill[Any](n) {new PairSpecialized[Foo, Bar](Foo(Random.nextInt(10)), Bar(Random.nextInt(10))) {}}
  val Ts_bar_str = Array.fill[Any](n) {new PairSpecialized[Bar, String](Bar(Random.nextInt(10)), "Hello") {}}

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
      res ^= state.Ms_str_foo(i).hashCode()
      res ^= state.Ms_foo_bar(i).hashCode()
      res ^= state.Ms_bar_str(i).hashCode()
      i += 1
    }
    blackHole.consume(res)

  @Benchmark
  def generic(state: Pairs, blackHole: Blackhole) =
    var i = 0
    var res = Random.nextInt(10)
    while (i < state.n) {
      res ^= state.As_str_foo(i).hashCode()
      res ^= state.As_foo_bar(i).hashCode()
      res ^= state.As_bar_str(i).hashCode()
      i += 1
    }
    blackHole.consume(res)

  @Benchmark
  def specialized(state: Pairs, blackHole: Blackhole) =
    var i = 0
    var res = Random.nextInt(10)
    while (i < state.n) {
      res ^= state.Ts_str_foo(i).asInstanceOf[PairSpecialized[String, Foo]].hashCode()
      res ^= state.Ts_foo_bar(i).asInstanceOf[PairSpecialized[Foo, Bar]].hashCode()
      res ^= state.Ts_bar_str(i).asInstanceOf[PairSpecialized[Bar, String]].hashCode()
      i += 1
    }
    blackHole.consume(res)
