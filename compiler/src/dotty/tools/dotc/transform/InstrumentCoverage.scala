package dotty.tools.dotc
package transform

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import collection.mutable
import core.Flags.*
import core.Contexts.{Context, ctx, inContext}
import core.DenotTransformers.IdentityDenotTransformer
import core.Symbols.{defn, Symbol}
import core.Decorators.{toTermName, i}
import core.Constants.Constant
import core.NameOps.isContextFunction
import core.Types.*
import typer.LiftCoverage
import util.{SourcePosition, Property}
import util.Spans.Span
import coverage.*

/** Implements code coverage by inserting calls to scala.runtime.coverage.Invoker
  * ("instruments" the source code).
  * The result can then be consumed by the Scoverage tool.
  */
class InstrumentCoverage extends MacroTransform with IdentityDenotTransformer:
  import ast.tpd._

  override def phaseName = InstrumentCoverage.name

  override def description = InstrumentCoverage.description

  // Enabled by argument "-coverage-out OUTPUT_DIR"
  override def isEnabled(using ctx: Context) =
    ctx.settings.coverageOutputDir.value.nonEmpty

  // counter to assign a unique id to each statement
  private var statementId = 0

  // stores all instrumented statements
  private val coverage = Coverage()

  override def run(using ctx: Context): Unit =
    val outputPath = ctx.settings.coverageOutputDir.value

    // Ensure the dir exists
    val dataDir = new File(outputPath)
    val newlyCreated = dataDir.mkdirs()

    if !newlyCreated then
      // If the directory existed before, let's clean it up.
      dataDir.listFiles.nn
        .filter(_.nn.getName.nn.startsWith("scoverage"))
        .foreach(_.nn.delete())
    end if
    super.run

    Serializer.serialize(coverage, outputPath, ctx.settings.sourceroot.value)

  override protected def newTransformer(using Context) = CoverageTransformer()

  /** Transforms trees to insert calls to Invoker.invoked to compute the coverage when the code is called */
  private class CoverageTransformer extends Transformer:
    private val IgnoreLiterals = new Property.Key[Boolean]

    private def ignoreLiteralsContext(using ctx: Context): Context =
      ctx.fresh.setProperty(IgnoreLiterals, true)

    override def transform(tree: Tree)(using ctx: Context): Tree =
      inContext(transformCtx(tree)) { // necessary to position inlined code properly
        tree match
          // simple cases
          case tree: (Import | Export | This | Super | New) => tree
          case tree if tree.isEmpty || tree.isType => tree // empty Thicket, Ident, TypTree, ...

          // Literals must be instrumented (at least) when returned by a def,
          // otherwise `def d = "literal"` is not covered when called from a test.
          // They can be left untouched when passed in a parameter of an Apply.
          case tree: Literal =>
            if ctx.property(IgnoreLiterals).contains(true) then
              tree
            else
              instrument(tree)

          // branches
          case tree: If =>
            cpy.If(tree)(
              cond = transform(tree.cond),
              thenp = instrument(transform(tree.thenp), branch = true),
              elsep = instrument(transform(tree.elsep), branch = true)
            )
          case tree: Try =>
            cpy.Try(tree)(
              expr = instrument(transform(tree.expr), branch = true),
              cases = instrumentCases(tree.cases),
              finalizer = instrument(transform(tree.finalizer), true)
            )

          // a.f(args)
          case tree @ Apply(fun: Select, args) =>
            // don't transform the first Select, but do transform `a.b` in `a.b.f(args)`
            if canInstrumentApply(tree) then
              val transformedFun = cpy.Select(fun)(transform(fun.qualifier), fun.name)
              if needsLift(tree) then
                val transformed = cpy.Apply(tree)(transformedFun, args) // args will be transformed in instrumentLifted
                instrumentLifted(transformed)(using ignoreLiteralsContext)
              else
                val transformed = cpy.Apply(tree)(transformedFun, transform(args)(using ignoreLiteralsContext))
                instrument(transformed)(using ignoreLiteralsContext)
            else
              tree

          // f(args)
          case tree: Apply =>
            if canInstrumentApply(tree) then
              if needsLift(tree) then
                instrumentLifted(tree)(using ignoreLiteralsContext) // see comment about Literals
              else
                instrument(super.transform(tree)(using ignoreLiteralsContext))
            else
              tree

          // (f(x))[args]
          case TypeApply(fun: Apply, args) =>
            cpy.TypeApply(tree)(transform(fun), args)

          // a.b
          case Select(qual, name) =>
            if qual.symbol.exists && qual.symbol.is(JavaDefined) then
              //Java class can't be used as a value, we can't instrument the
              //qualifier ({<Probe>;System}.xyz() is not possible !) instrument it
              //as it is
              instrument(tree)
            else
              val transformed = cpy.Select(tree)(transform(qual), name)
              if transformed.qualifier.isDef then
                // instrument calls to methods without parameter list
                instrument(transformed)
              else
                transformed

          case tree: CaseDef => instrumentCaseDef(tree)
          case tree: ValDef =>
            // only transform the rhs
            val rhs = transform(tree.rhs)
            cpy.ValDef(tree)(rhs=rhs)

          case tree: DefDef =>
            // only transform the params (for the default values) and the rhs
            // force instrumentation of literals and other small trees in the rhs,
            // to ensure that the method call are recorded
            val paramss = transformParamss(tree.paramss)
            val rhs = transform(tree.rhs)
            cpy.DefDef(tree)(tree.name, paramss, tree.tpt, rhs)

          case tree: PackageDef =>
            // only transform the statements of the package
            cpy.PackageDef(tree)(tree.pid, transform(tree.stats))
          case tree: Assign =>
            // only transform the rhs
            cpy.Assign(tree)(tree.lhs, transform(tree.rhs))

          // For everything else just recurse and transform
          // Special care for Templates: it's important to set the owner of the `stats`, like super.transform
          case _ =>
            super.transform(tree)
        }

    /** Lifts and instruments an application.
      * Note that if only one arg needs to be lifted, we just lift everything.
      */
    def instrumentLifted(tree: Apply)(using Context) =
      // lifting
      val buffer = mutable.ListBuffer[Tree]()
      val liftedApply = LiftCoverage.liftForCoverage(buffer, tree)

      // instrumentation
      val instrumentedArgs = buffer.toList.map(transform)
      val instrumentedApply = instrument(liftedApply)
      Block(
        instrumentedArgs,
        instrumentedApply
      ).withSpan(instrumentedApply.span)

    def instrumentCases(cases: List[CaseDef])(using Context): List[CaseDef] =
      cases.map(instrumentCaseDef)

    def instrumentCaseDef(tree: CaseDef)(using Context): CaseDef =
      cpy.CaseDef(tree)(tree.pat, transform(tree.guard), transform(tree.body))

    def instrument(tree: Tree, branch: Boolean = false)(using Context): Tree =
      instrument(tree, tree.sourcePos, branch)

    def instrument(tree: Tree, pos: SourcePosition, branch: Boolean)(using ctx: Context): Tree =
      if pos.exists && !pos.span.isZeroExtent then
        statementId += 1
        val id = statementId
        val statement = new Statement(
          source = ctx.source.file.name,
          location = Location(tree),
          id = id,
          start = pos.start,
          end = pos.end,
          line = pos.line,
          desc = tree.source.content.slice(pos.start, pos.end).mkString,
          symbolName = tree.symbol.name.toSimpleName.toString(),
          treeName = tree.getClass.getSimpleName.nn,
          branch
        )
        coverage.addStatement(statement)
        val span = Span(pos.start, pos.end) // synthetic span
        Block(List(invokeCall(id, span)), tree).withSpan(span)
      else
        tree

    def invokeCall(id: Int, span: Span)(using Context): Tree =
      val outputPath = ctx.settings.coverageOutputDir.value
      ref(defn.InvokedMethodRef).withSpan(span)
        .appliedToArgs(
          List(Literal(Constant(id)), Literal(Constant(outputPath)))
        ).withSpan(span)

    /**
     * Checks if the apply needs a lift in the coverage phase.
     * In case of a nested application, we have to lift all arguments
     * Example:
     * ```
     * def T(x:Int)(y:Int)
     * T(f())(1)
     * ```
     * should not be changed to {val $x = f(); T($x)}(1) but to {val $x = f(); val $y = 1; T($x)($y)}
     */
    def needsLift(tree: Apply)(using Context): Boolean =
      def isBooleanOperator(fun: Tree) =
        // We don't want to lift a || getB(), to avoid calling getB if a is true.
        // Same idea with a && getB(): if a is false, getB shouldn't be called.
        val sym = fun.symbol
        sym.exists &&
        sym == defn.Boolean_&& || sym == defn.Boolean_||

      def isContextual(fun: Apply): Boolean =
        val args = fun.args
        args.nonEmpty && args.head.symbol.isAllOf(Given | Implicit)

      val fun = tree.fun
      val nestedApplyNeedsLift = fun match
        case a: Apply => needsLift(a)
        case _ => false

      nestedApplyNeedsLift ||
      !isBooleanOperator(fun) && !tree.args.isEmpty && !tree.args.forall(LiftCoverage.noLift)

    def canInstrumentApply(tree: Apply)(using Context): Boolean =
      val tpe = tree.typeOpt
      tpe match
        case AppliedType(tycon: NamedType, _) =>
          /* If the last expression in a block is a context function, we'll try to
             summon its arguments at the current point, even if the expected type
             is a function application. Therefore, this is not valid:
            ```
            def f = (t: Exception) ?=> (c: String) ?=> result

            ({
              invoked()
              f(using e)
            })(using s)
            ```
          */
          !tycon.name.isContextFunction
        case m: MethodType =>
          // def f(a: Ta)(b: Tb)
          // f(a)(b) cannot be rewritten to {invoked();f(a)}(b)
          false
        case _ =>
          true

object InstrumentCoverage:
  val name: String = "instrumentCoverage"
  val description: String = "instrument code for coverage cheking"
