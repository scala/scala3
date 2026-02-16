package dotty.tools.benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Level, Measurement, Mode as JMHMode, Param, Scope, Setup, State, Warmup}
import java.util.concurrent.TimeUnit.SECONDS

import dotty.tools.dotc.{Driver, Run, Compiler}
import dotty.tools.dotc.ast.{tpd, TreeTypeMap}, tpd.{Apply, Block, Tree, TreeAccumulator, TypeApply}
import dotty.tools.dotc.core.Annotations.{Annotation, ConcreteAnnotation, EmptyAnnotation}
import dotty.tools.dotc.core.Contexts.{ContextBase, Context, ctx, withMode}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.{defn, mapSymbols, Symbol}
import dotty.tools.dotc.core.Types.{AnnotatedType, NoType, SkolemType, TermRef, Type, TypeMap}
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.typer.TyperPhase

/** Measures the performance of mapping over annotated types.
  *
  * Run with: scala3-bench-micro / Jmh / run AnnotationsMappingBenchmark
  */
@Fork(value = 4)
@Warmup(iterations = 4, time = 1, timeUnit = SECONDS)
@Measurement(iterations = 4, time = 1, timeUnit = SECONDS)
@BenchmarkMode(Array(JMHMode.Throughput))
@State(Scope.Thread)
class AnnotationsMappingBenchmark:
  var tp: Type = null
  var specialIntTp: Type = null
  var context: Context = null
  var typeFunction: Context ?=> Type => Type = null
  var typeMap: TypeMap = null

  @Param(Array("v1", "v2", "v3", "v4"))
  var valName: String = null

  @Param(Array("id", "mapInts"))
  var typeFunctionName: String = null

  @Setup(Level.Iteration)
  def setup(): Unit =
    val testPhase =
      new Phase:
        final override def phaseName = "testPhase"
        final override def run(using ctx: Context): Unit =
          val pkg = ctx.compilationUnit.tpdTree.symbol
          tp = pkg.requiredClass("Test").requiredValueRef(valName).underlying
          specialIntTp = pkg.requiredClass("Test").requiredType("SpecialInt").typeRef
          context = ctx

    val compiler =
      new Compiler:
        private final val baseCompiler = new Compiler()
        final override def phases = List(List(Parser()), List(TyperPhase()), List(testPhase))

    val driver =
      new Driver:
        final override def newCompiler(using Context): Compiler = compiler

    driver.process(Array("-classpath", System.getProperty("BENCH_CLASS_PATH"), "tests/someAnnotatedTypes.scala"))

    typeFunction =
      typeFunctionName match
        case "id"      => tp => tp
        case "mapInts" => tp => (if tp frozen_=:= defn.IntType then specialIntTp else tp)
        case _         => throw new IllegalArgumentException(s"Unknown type function: $typeFunctionName")

    typeMap =
      new TypeMap(using context):
        final override def apply(tp: Type): Type = typeFunction(mapOver(tp))

  @Benchmark def applyTypeMap() = typeMap.apply(tp)
