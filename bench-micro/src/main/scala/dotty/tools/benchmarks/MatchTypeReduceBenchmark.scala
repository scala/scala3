package dotty.tools.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit.NANOSECONDS
import dotty.tools.dotc.{Driver, Run, Compiler}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Types.{MatchType, Type, TypeRef, AliasingBounds, HKTypeLambda}
import dotty.tools.dotc.core.Contexts.{Context, ctx, withMode}

/** Measures the cost of a cache hit in `MatchType.reduced`.
 *
 *  Drives `MatchType.reduced` repeatedly against a stuck match type whose case
 *  body mentions many class type parameters (see
 *  `bench-micro/tests/matchTypeHeavyBody.scala`). Every call revalidates the
 *  cached reduction through `changedReductionContext`, which iterates the
 *  reduction-context footprint, so this measures how much the footprint costs
 *  per call.
 *
 *  None of the types in that case body can invalidate the cached reduction, so
 *  a correct footprint stays empty here and the benchmark stays flat.
 *  Recording them anyway — for instance by deep-traversing case bodies and
 *  adding every `TypeParam`-tagged `NamedType` found there — costs roughly an
 *  order of magnitude per call (~17 ns/call against ~173 ns/call, measured
 *  in-process over 5 runs of 2_000_000 iterations each).
 *
 *  The bench-micro project is currently disabled in `project/Build.scala`
 *  (`scala3-bench-micro` is commented out). Running this requires enabling
 *  that project.
 */
@Fork(value = 5)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(NANOSECONDS)
@State(Scope.Thread)
class MatchTypeReduceBenchmark:
  var mt: MatchType = null
  var context: Context = null

  @Setup(Level.Iteration)
  def setup(): Unit =
    val driver = new Driver:
      override def finish(compiler: Compiler, run: Run)(using Context): Unit =
        withMode(Mode.Printing) {
          val pkg = run.units(0).tpdTree.symbol
          val outerCls = pkg.requiredClass("Outer")
          val heavyRef = outerCls.requiredValueRef("matchTypeHeavyBody")
          heavyRef.underlying.widen match
            case tr: TypeRef =>
              tr.info match
                case ab: AliasingBounds =>
                  ab.alias match
                    case found: MatchType => mt = found
                    case _                =>
                case _ =>
            case found: MatchType => mt = found
            case _                =>
          context = ctx
        }
        super.finish(compiler, run)
    driver.process(Array(
      "-classpath", System.getProperty("BENCH_CLASS_PATH"),
      "-Ystop-after:typer",
      "tests/matchTypeHeavyBody.scala"
    ))

  @Benchmark
  def reduced(): Type = mt.reduced(using context)
