package dotty.tools.benchmarks.lazyvals

import compiletime.uninitialized
import org.openjdk.jmh.annotations._
import LazyVals.LazyHolder
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import java.util.concurrent.{Executors, ExecutorService}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class ContendedInitialization {

  @Param(Array("2000000", "5000000"))
  var size: Int = uninitialized

  @Param(Array("2", "4", "8"))
  var nThreads: Int = uninitialized

  var executor: ExecutorService = uninitialized

  @Setup
  def prepare: Unit = {
    executor = Executors.newFixedThreadPool(nThreads)
  }

  @TearDown
  def cleanup: Unit = {
    executor.shutdown()
    executor = null
  }

  @Benchmark
  def measureContended(bh: Blackhole): Unit = {
    val array = Array.fill(size)(new LazyHolder)
    val task: Runnable = () =>
    for (elem <- array) bh.consume(elem.value)

    val futures =
    for (_ <- 0 until nThreads) yield
      executor.submit(task)

    futures.foreach(_.get())
  }
}
