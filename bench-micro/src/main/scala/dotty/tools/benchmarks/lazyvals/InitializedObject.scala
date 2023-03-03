package dotty.tools.benchmarks.lazyvals

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import LazyVals.ObjectHolder
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class InitializedObject {

  @Benchmark
  def measureInitialized(bh: Blackhole) = {
    bh.consume(ObjectHolder)
    bh.consume(ObjectHolder.value)
  }
}
