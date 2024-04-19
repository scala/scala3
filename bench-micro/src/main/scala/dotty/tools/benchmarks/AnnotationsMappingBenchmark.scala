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

/** Benchmark to compare different ways to map concrete annotations.
  *
  * The main performance bottleneck there is the mapping of the annotation's
  * inner tree of; a `TreeTypeMap` is much slower than a `TypeMap`.
  *
  * Run with: scala3-bench-micro / Jmh / run AnnotationsMappingBenchmark
  */
@Fork(value = 5)
// Set to 0 to record all iterations. We remove the first iterations manually
// when processing the results.
@Warmup(iterations = 0, time = 1, timeUnit = SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = SECONDS)
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

  @Param(Array("current", "oldCheck", "newCheckEquals", "newCheckEq", "noCheck", "noCheckCopySymbols"))
  var typeMapName: String = null

  @Param(Array("id", "mapInts"))
  var typeFunctionName: String = null

  @Setup(Level.Iteration)
  def setup(): Unit =
    /** A custom phase that is used to retrieve the `Type`s and `Context` to be
      * used in the benchmark.
      */
    val testPhase =
      new Phase:
        final override def phaseName = "testPhase"
        final override def run(using ctx: Context): Unit =
          val pkg = ctx.compilationUnit.tpdTree.symbol
          tp = pkg.requiredClass("Test").requiredValueRef(valName).underlying
          specialIntTp = pkg.requiredClass("Test").requiredType("SpecialInt").typeRef
          context = ctx

    /** A custom compiler that only runs the `Parser`, `TyperPhase` and
      * `testPhase`.
      */
    val compiler =
      new Compiler:
        private final val baseCompiler = new Compiler()
        final override def phases = List(List(Parser()), List(TyperPhase()), List(testPhase))

    /** A custom driver that uses `compiler`. */
    val driver =
      new Driver:
        final override def newCompiler(using Context): Compiler = compiler

    // Runs the driver with the test file.
    driver.process(Array("-classpath", System.getProperty("BENCH_CLASS_PATH"), "tests/someAnnotatedTypes.scala"))

    typeFunction =
      typeFunctionName match
        case "id"      => tp => tp
        case "mapInts" => tp => (if tp frozen_=:= defn.IntType then specialIntTp else tp)
        case _         => throw new IllegalArgumentException(s"Unknown type function: $typeFunctionName")

    /** Creates a new `TypeMap` that uses `mapConcreteAnnotationWith` to map
      * concrete annotations. It is used to compare several ways to map these
      * annotations.
      */
    def makeTypeMap(mapConcreteAnnotationWith: (ConcreteAnnotation, TypeMap) => Context ?=> Annotation) =
      new TypeMap(using context):
        final override def apply(tp: Type): Type = typeFunction(mapOver(tp))
        final override def mapOver(tp: Type) =
          tp match
            case tp @ AnnotatedType(underlying, annot) =>
              val underlying1 = this(underlying)
              val annot1 =
                annot match
                  case annot: ConcreteAnnotation => mapConcreteAnnotationWith(annot, this)
                  case _                         => annot.mapWith(this)
              if annot1 eq EmptyAnnotation then underlying1
              else derivedAnnotatedType(tp, underlying1, annot1)
            case _ => super.mapOver(tp)

    /** Retrieves all argument from a tree. This old implementation does not
      * include type arguments.
      */
    def oldAllArguments(tree: Tree)(using Context): List[Tree] =
      tpd.unsplice(tree) match
        case Apply(fn, args)  => oldAllArguments(fn) ::: args
        case TypeApply(fn, _) => oldAllArguments(fn)
        case Block(_, expr)   => oldAllArguments(expr)
        case _                => Nil

    /** This is the old (<= d1489734b7) implementation of `Annotation.mapWith`.
      * It 1. does not include type arguments and 2. uses `frozen_=:=` to
      * compare types and 3. does not copy all symbols.
      */
    def oldMapWith(annot: ConcreteAnnotation, tm: TypeMap)(using Context): Annotation =
      val tree = annot.tree
      val args = oldAllArguments(tree)
      if args.isEmpty then annot
      else
        val findDiff = new TreeAccumulator[Type]:
          def apply(x: Type, tree: Tree)(using Context): Type =
            if tm.isRange(x) then x
            else
              val tp1 = tm(tree.tpe)
              foldOver(if tp1 frozen_=:= tree.tpe then x else tp1, tree)
        val diff = findDiff(NoType, args)
        if tm.isRange(diff) then EmptyAnnotation
        else if diff.exists then annot.derivedAnnotation(tm.mapOver(tree))
        else annot

    /** Retrieves all argument from a tree, including type arguments. */
    def newAllArguments(tree: Tree)(using Context): List[Tree] =
      tpd.unsplice(tree) match
        case Apply(fn, args)     => newAllArguments(fn) ::: args
        case TypeApply(fn, args) => newAllArguments(fn) ::: args
        case Block(_, expr)      => newAllArguments(expr)
        case _                   => Nil

    /** This is the new implementation of `Annotation.mapWith`. It 1. includes
      * type arguments and 2. uses `==` to compare types and 3. copies all
      * symbols by using a custom `TreeTypeMap` that overrides `withMappedSyms`.
      */
    def newMapWithEquals(annot: ConcreteAnnotation, tm: TypeMap)(using Context): Annotation =
      val tree = annot.tree
      val args = newAllArguments(tree)
      if args.isEmpty then annot
      else
        val findDiff = new TreeAccumulator[Type]:
          def apply(x: Type, tree: Tree)(using Context): Type =
            if tm.isRange(x) then x
            else
              val tp1 = tm(tree.tpe)
              foldOver(if tp1 == tree.tpe then x else tp1, tree)
        val diff = findDiff(NoType, args)
        if tm.isRange(diff) then EmptyAnnotation
        else if diff.exists then
          val ttm =
            new TreeTypeMap(tm):
              final override def withMappedSyms(syms: List[Symbol]): TreeTypeMap =
                withMappedSyms(syms, mapSymbols(syms, this, mapAlways = true))
          annot.derivedAnnotation(ttm.transform(tree))
        else annot

    /** Exactly the same as `newMapWithEquals`, but uses `eq` instead of `==` to
      * compare types.
      */
    def newMapWithEq(annot: ConcreteAnnotation, tm: TypeMap)(using Context): Annotation =
      val tree = annot.tree
      val args = newAllArguments(tree)
      if args.isEmpty then annot
      else
        val findDiff = new TreeAccumulator[Type]:
          def apply(x: Type, tree: Tree)(using Context): Type =
            if tm.isRange(x) then x
            else
              val tp1 = tm(tree.tpe)
              foldOver(if tp1 eq tree.tpe then x else tp1, tree)
        val diff = findDiff(NoType, args)
        if tm.isRange(diff) then EmptyAnnotation
        else if diff.exists then
          val ttm =
            new TreeTypeMap(tm):
              final override def withMappedSyms(syms: List[Symbol]): TreeTypeMap =
                withMappedSyms(syms, mapSymbols(syms, this, mapAlways = true))
          annot.derivedAnnotation(ttm.transform(tree))
        else annot

    def noCheckMapWith(annot: ConcreteAnnotation, tm: TypeMap)(using Context): Annotation =
      annot.derivedAnnotation(tm.mapOver(annot.tree))

    def noCheckCopySymbolsMapWith(annot: ConcreteAnnotation, tm: TypeMap)(using Context): Annotation =
      val ttm =
        new TreeTypeMap(tm):
          final override def withMappedSyms(syms: List[Symbol]): TreeTypeMap =
            withMappedSyms(syms, mapSymbols(syms, this, mapAlways = true))
      annot.derivedAnnotation(ttm.transform(annot.tree))

    typeMap =
      typeMapName match
        case "current" =>
          new TypeMap(using context):
            final override def apply(tp: Type): Type = typeFunction(mapOver(tp))
        case "oldCheck" =>
          makeTypeMap(oldMapWith)
        case "newCheckEquals" =>
          // This should be the same as `current`, modulo a few indirections.
          makeTypeMap(newMapWithEq)
        case "newCheckEq" =>
          makeTypeMap(newMapWithEq)
        case "noCheck" =>
          makeTypeMap(noCheckMapWith)
        case "noCheckCopySymbols" =>
          makeTypeMap(noCheckCopySymbolsMapWith)
        case _ =>
          throw new IllegalArgumentException(s"Unknown type map: $typeMapName")

  @Benchmark
  def applyTypeMap() =
    val res = typeMap.apply(tp)
    // println(res.show(using context))
