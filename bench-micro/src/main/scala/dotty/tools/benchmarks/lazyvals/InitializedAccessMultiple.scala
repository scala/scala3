package dotty.tools.benchmarks.lazyvals

import compiletime.uninitialized
import org.openjdk.jmh.annotations._
import LazyVals.LazyHolder
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class InitializedAccessMultiple {

  var holders: Array[LazyHolder] = uninitialized

  @Setup
  def prepare: Unit = {
    holders = Array.fill(100){ new LazyHolder }
  }

  @Benchmark
  def measureInitialized(bh: Blackhole) = {
    var i = 0
    while(i < 100) {
      val currentHolder = holders(i)
      bh.consume(currentHolder)
      bh.consume(currentHolder.value)
      i = i + 1
    }
  }
}
