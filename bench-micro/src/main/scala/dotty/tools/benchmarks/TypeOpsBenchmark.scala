package dotty.tools.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit.SECONDS
import dotty.tools.dotc.{Driver, Run, Compiler}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.core.Contexts.{ContextBase, Context, ctx, withMode}

@Fork(value = 5)
@Warmup(iterations = 5, time = 1, timeUnit = SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = SECONDS)
@State(Scope.Thread)
class TypeOpsBenchmark:
  var tp: Type = null
  var context: Context = null

  @Param(Array("int", "singletonsSum", "intsSum", "deepSingletonsSum", "deepIntsSum", "singletonsIntersection", "singletonsUnion"))
  var valName: String = "int"

  @Setup(Level.Iteration)
  def setup(): Unit =
    val driver = new Driver:
      override def finish(compiler: Compiler, run: Run)(using Context): Unit =
        withMode(Mode.Printing) {
          val pkg = run.units(0).tpdTree.symbol
          tp = pkg.requiredClass("Test").requiredValueRef(valName).underlying
          context = ctx
        }
        super.finish(compiler, run)
    driver.process(Array(
      "-classpath", System.getProperty("BENCH_CLASS_PATH"),
      "-Ystop-after:typer",
      "tests/someTypes.scala"
    ))

  @Benchmark
  def isStable(): Unit = tp.isStable(using context)

  @Benchmark
  def normalized(): Unit = tp.normalized(using context)

  @Benchmark
  def simplified(): Unit = tp.simplified(using context)

  @Benchmark
  def dealias(): Unit = tp.dealias(using context)

  @Benchmark
  def widen(): Unit = tp.widen(using context)

  @Benchmark
  def atoms(): Unit = tp.atoms(using context)

  @Benchmark
  def isProvisional(): Unit = tp.isProvisional(using context)
