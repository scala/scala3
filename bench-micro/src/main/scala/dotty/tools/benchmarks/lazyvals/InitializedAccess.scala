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
class InitializedAccess {

  var holder: LazyHolder = uninitialized

  @Setup
  def prepare: Unit = {
    holder = new LazyHolder
    holder.value
  }

  @Benchmark
  def measureInitialized(bh: Blackhole) = {
    bh.consume(holder)
    bh.consume(holder.value)
  }
}
