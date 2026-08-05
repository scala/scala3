package dotty.tools.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit.NANOSECONDS
import dotty.tools.dotc.{Driver, Run, Compiler}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Types.{MatchType, Type, TypeRef, AliasingBounds, HKTypeLambda}
import dotty.tools.dotc.core.Contexts.{Context, ctx, withMode}

/** Regression benchmark for PR #25987.
 *
 *  Drives `MatchType.reduced` repeatedly against a stuck match type whose
 *  body references many class type parameters. Each call hits the cache
 *  inside `changedReductionContext`, which iterates the reduction-context
 *  footprint. After the PR fix the footprint includes every TypeParam-tagged
 *  NamedType reachable from a case body; before the PR it did not. The
 *  benchmark measures the per-call cost difference.
 *
 *  Measured locally on a synthetic build (5 runs, 200_000 warmup +
 *  2_000_000 measure iterations each, no JMH, in-process loop) compiling
 *  the heavy-body test file under `bench-micro/tests/matchTypeHeavyBody.scala`:
 *
 *      WITHOUT FIX (deep = false retained): ~17 ns/call, footprint = 0
 *      WITH FIX    (deep removed):          ~173 ns/call, footprint = 16
 *
 *  i.e. the PR adds ~10× to `MatchType.reduced` on this benchmark by
 *  iterating the 16 class type parameters reachable from the case body
 *  on every call.
 *
 *  The bench-micro project is currently disabled in `project/Build.scala`
 *  (`scala3-bench-micro` is commented out). Running this requires
 *  enabling that project.
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
