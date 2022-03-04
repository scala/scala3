package dotty.tools.dotc
package transform

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import collection.mutable
import core.Flags.JavaDefined
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.coverage.Coverage
import dotty.tools.dotc.coverage.Statement
import dotty.tools.dotc.coverage.Serializer
import dotty.tools.dotc.coverage.Location
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.typer.LiftCoverage

import scala.quoted

/** Implements code coverage by inserting calls to scala.runtime.Invoker
  * ("instruments" the source code).
  * The result can then be consumed by the Scoverage tool.
  */
class InstrumentCoverage extends MacroTransform with IdentityDenotTransformer:
  import ast.tpd._

  override def phaseName = InstrumentCoverage.name

  override def description = InstrumentCoverage.description

  // Enabled by argument "-coverage OUTPUT_DIR"
  override def isEnabled(using ctx: Context) =
    ctx.settings.coverageOutputDir.value.nonEmpty

  // Atomic counter used for assignation of IDs to difference statements
  private val statementId = AtomicInteger(0)

  private var outputPath = ""

  // Main class used to store all instrumented statements
  private val coverage = Coverage()

  override def run(using ctx: Context): Unit =
    outputPath = ctx.settings.coverageOutputDir.value

    // Ensure the dir exists
    val dataDir = new File(outputPath)
    val newlyCreated = dataDir.mkdirs()

    if (!newlyCreated) {
      // If the directory existed before, let's clean it up.
      dataDir.listFiles
        .filter(_.getName.startsWith("scoverage"))
        .foreach(_.delete)
    }

    super.run

    Serializer.serialize(coverage, outputPath, ctx.settings.coverageSourceroot.value)

  override protected def newTransformer(using Context) = CoverageTransormer()

  private class CoverageTransormer extends Transformer:

    override def transform(tree: Tree)(using Context): Tree =
      println(tree.show + tree.toString)
      tree match
        // simple cases
        case tree: (Literal | Import | Export) => tree
        case tree: (New | This | Super) => instrument(tree)
        case tree if (tree.isEmpty || tree.isType) => tree // empty Thicket, Ident, TypTree, ...

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

        // f(args)
        case tree: Apply =>
          if needsLift(tree) then
            liftApply(tree)
          else
            super.transform(tree)

        // (f(x))[args]
        case tree @ TypeApply(fun: Apply, args) =>
          cpy.TypeApply(tree)(transform(fun), args)

        // a.b
        case Select(qual, _) if (qual.symbol.exists && qual.symbol.is(JavaDefined)) =>
          //Java class can't be used as a value, we can't instrument the
          //qualifier ({<Probe>;System}.xyz() is not possible !) instrument it
          //as it is
          instrument(tree)
        case tree: Select =>
          if tree.qualifier.isInstanceOf[New] then
            instrument(tree)
          else
            cpy.Select(tree)(transform(tree.qualifier), tree.name)

        case tree: CaseDef => instrumentCaseDef(tree)
        case tree: ValDef =>
          // only transform the rhs
          cpy.ValDef(tree)(rhs = transform(tree.rhs))

        case tree: PackageDef =>
          // only transform the statements of the package
          cpy.PackageDef(tree)(tree.pid, transform(tree.stats))
        case tree: Assign =>
          cpy.Assign(tree)(tree.lhs, transform(tree.rhs))
        case tree: Template =>
          // Don't instrument the parents (extends) of a template since it
          // causes problems if the parent constructor takes parameters
          cpy.Template(tree)(
            constr = super.transformSub(tree.constr),
            body = transform(tree.body)
          )

        // For everything else just recurse and transform
        case _ =>
          report.warning(
            "Unmatched: " + tree.getClass + " " + tree.symbol,
             tree.sourcePos
          )
          super.transform(tree)

    def liftApply(tree: Apply)(using Context) =
      val buffer = mutable.ListBuffer[Tree]()
      // NOTE: that if only one arg needs to be lifted, we just lift everything
      val lifted = LiftCoverage.liftForCoverage(buffer, tree)
      val instrumented = buffer.toList.map(transform)
      // We can now instrument the apply as it is with a custom position to point to the function
      Block(
        instrumented,
        instrument(
          lifted,
          tree.sourcePos,
          false
        )
      )

    def instrumentCases(cases: List[CaseDef])(using Context): List[CaseDef] =
      cases.map(instrumentCaseDef)

    def instrumentCaseDef(tree: CaseDef)(using Context): CaseDef =
      cpy.CaseDef(tree)(tree.pat, transform(tree.guard), transform(tree.body))

    def instrument(tree: Tree, branch: Boolean = false)(using Context): Tree =
      instrument(tree, tree.sourcePos, branch)

    def instrument(tree: Tree, pos: SourcePosition, branch: Boolean)(using ctx: Context): Tree =
      if pos.exists && !pos.span.isZeroExtent then
        val id = statementId.incrementAndGet()
        val statement = new Statement(
          source = ctx.source.file.name,
          location = Location(tree),
          id = id,
          start = pos.start,
          end = pos.end,
          line = ctx.source.offsetToLine(pos.point),
          desc = tree.source.content.slice(pos.start, pos.end).mkString,
          symbolName = tree.symbol.name.toSimpleName.toString(),
          treeName = tree.getClass.getSimpleName,
          branch
        )
        coverage.addStatement(statement)
        Block(List(invokeCall(id)), tree)
      else
        tree

    def invokeCall(id: Int)(using Context): Tree =
      ref(defn.InvokerModuleRef)
        .select("invoked".toTermName)
        .appliedToArgs(
          List(Literal(Constant(id)), Literal(Constant(outputPath)))
        )

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
      // We don't want to lift a || getB(), to avoid calling getB if a is true.
      // Same idea with a && getB(): if a is false, getB shouldn't be called.
      def isBooleanOperator(fun: Tree) =
        fun.symbol.exists &&
        fun.symbol.isInstanceOf[Symbol] &&
        fun.symbol == defn.Boolean_&& || fun.symbol == defn.Boolean_||

      val fun = tree.fun

      fun.isInstanceOf[Apply] || // nested apply
      !isBooleanOperator(fun) ||
      !tree.args.isEmpty && !tree.args.forall(LiftCoverage.noLift)

object InstrumentCoverage:
  val name: String = "instrumentCoverage"
  val description: String = "instrument code for coverage cheking"
