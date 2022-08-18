package dotty.tools.dotc
package transform

import java.io.File

import ast.tpd.*
import collection.mutable
import core.Flags.*
import core.Contexts.{Context, ctx, inContext}
import core.DenotTransformers.IdentityDenotTransformer
import core.Symbols.{defn, Symbol}
import core.Constants.Constant
import core.NameOps.isContextFunction
import core.Types.*
import coverage.*
import typer.LiftCoverage
import util.SourcePosition
import util.Spans.Span
import localopt.StringInterpolatorOpt

/** Implements code coverage by inserting calls to scala.runtime.coverage.Invoker
  * ("instruments" the source code).
  * The result can then be consumed by the Scoverage tool.
  */
class InstrumentCoverage extends MacroTransform with IdentityDenotTransformer:

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
    val dataDir = File(outputPath)
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
    override def transform(tree: Tree)(using Context): Tree =
      inContext(transformCtx(tree)) { // necessary to position inlined code properly
        tree match
          // simple cases
          case tree: (Import | Export | Literal | This | Super | New) => tree
          case tree if tree.isEmpty || tree.isType => tree // empty Thicket, Ident (referring to a type), TypeTree, ...

          // identifier
          case tree: Ident =>
            val sym = tree.symbol
            if canInstrumentParameterless(sym) then
              // call to a local parameterless method f
              instrument(tree)
            else
              tree

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
              finalizer = instrument(transform(tree.finalizer), branch = true)
            )

          // f(args)
          case tree: Apply =>
            if canInstrumentApply(tree) then
              if needsLift(tree) then
                instrumentLifted(tree)
              else
                instrument(transformApply(tree))
            else
              transformApply(tree)

          // (fun)[args]
          case TypeApply(fun, args) =>
            val tfun = transform(fun)
            tfun match
              case InstrumentCoverage.InstrumentedBlock(invokeCall, expr) =>
                // expr[T] shouldn't be transformed to
                // {invoked(...), expr}[T]
                //
                // but to
                // {invoked(...), expr[T]}
                //
                // This is especially important for trees like (expr[T])(args),
                // for which the wrong transformation crashes the compiler.
                // See tests/coverage/pos/PolymorphicExtensions.scala
                Block(
                  invokeCall :: Nil,
                  cpy.TypeApply(tree)(expr, args)
                )
              case _ =>
                cpy.TypeApply(tree)(tfun, args)

          // a.b
          case Select(qual, name) =>
            val transformed = cpy.Select(tree)(transform(qual), name)
            val sym = tree.symbol
            if canInstrumentParameterless(sym) then
              // call to a parameterless method
              instrument(transformed)
            else
              transformed

          case tree: CaseDef => instrumentCaseDef(tree)
          case tree: ValDef =>
            // only transform the rhs
            val rhs = transform(tree.rhs)
            cpy.ValDef(tree)(rhs = rhs)

          case tree: DefDef =>
            if tree.symbol.isOneOf(Inline | Erased) then
              // Inline and erased definitions will not be in the generated code and therefore do not need to be instrumented.
              // Note that a retained inline method will have a `$retained` variant that will be instrumented.
              tree
            else
              // Only transform the params (for the default values) and the rhs.
              val paramss = transformParamss(tree.paramss)
              val rhs = transform(tree.rhs)
              val finalRhs =
                if canInstrumentDefDef(tree) then
                  // Ensure that the rhs is always instrumented, if possible.
                  // This is useful because methods can be stored and called later, or called by reflection,
                  // and if the rhs is too simple to be instrumented (like `def f = this`), the method won't show up as covered.
                  instrumentBody(tree, rhs)
                else
                  rhs
              cpy.DefDef(tree)(tree.name, paramss, tree.tpt, finalRhs)
            end if
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
      * Note that if only one arg needs to be lifted, we just lift everything (see LiftCoverage).
      */
    private def instrumentLifted(tree: Apply)(using Context) =
      // lifting
      val buffer = mutable.ListBuffer[Tree]()
      val liftedApply = LiftCoverage.liftForCoverage(buffer, tree)

      // instrumentation
      val instrumentedArgs = buffer.toList.map(transform)
      val instrumentedApply = instrument(liftedApply)
      Block(
        instrumentedArgs,
        instrumentedApply
      )

    private inline def transformApply(tree: Apply)(using Context): Apply =
      cpy.Apply(tree)(transform(tree.fun), transform(tree.args))

    private inline def instrumentCases(cases: List[CaseDef])(using Context): List[CaseDef] =
      cases.map(instrumentCaseDef)

    private def instrumentCaseDef(tree: CaseDef)(using Context): CaseDef =
      val pat = tree.pat
      val guard = tree.guard
      val friendlyEnd = if guard.span.exists then guard.span.end else pat.span.end
      val pos = tree.sourcePos.withSpan(tree.span.withEnd(friendlyEnd)) // user-friendly span
      // ensure that the body is always instrumented by inserting a call to Invoker.invoked at its beginning
      val instrumentedBody = instrument(transform(tree.body), pos, false)
      cpy.CaseDef(tree)(tree.pat, transform(tree.guard), instrumentedBody)

    /** Records information about a new coverable statement. Generates a unique id for it.
      * @return the statement's id
      */
    private def recordStatement(tree: Tree, pos: SourcePosition, branch: Boolean)(using ctx: Context): Int =
      val id = statementId
      statementId += 1
      val statement = Statement(
        source = ctx.source.file.name,
        location = Location(tree),
        id = id,
        start = pos.start,
        end = pos.end,
        line = pos.line,
        desc = tree.source.content.slice(pos.start, pos.end).mkString,
        symbolName = tree.symbol.name.toSimpleName.toString,
        treeName = tree.getClass.getSimpleName.nn,
        branch
      )
      coverage.addStatement(statement)
      id

    private inline def syntheticSpan(pos: SourcePosition): Span = pos.span.toSynthetic

    /** Shortcut for instrument(tree, tree.sourcePos, branch) */
    private inline def instrument(tree: Tree, branch: Boolean = false)(using Context): Tree =
      instrument(tree, tree.sourcePos, branch)

    /** Instruments a statement, if it has a position. */
    private def instrument(tree: Tree, pos: SourcePosition, branch: Boolean)(using Context): Tree =
      if pos.exists && !pos.span.isZeroExtent then
        val statementId = recordStatement(tree, pos, branch)
        insertInvokeCall(tree, pos, statementId)
      else
        tree

    /** Instruments the body of a DefDef. Handles corner cases. */
    private def instrumentBody(parent: DefDef, body: Tree)(using Context): Tree =
      /* recurse on closures, so that we insert the call at the leaf:

         def g: (a: Ta) ?=> (b: Tb) = {
           // nothing here             <-- not here!
           def $anonfun(using a: Ta) =
             Invoked.invoked(id, DIR)  <-- here
             <userCode>
           closure($anonfun)
         }
      */
      body match
        case b @ Block((meth: DefDef) :: Nil, closure: Closure)
        if meth.symbol == closure.meth.symbol && defn.isContextFunctionType(body.tpe) =>
          val instr = cpy.DefDef(meth)(rhs = instrumentBody(parent, meth.rhs))
          cpy.Block(b)(instr :: Nil, closure)
        case _ =>
          // compute user-friendly position to highlight more text in the coverage UI
          val namePos = parent.namePos
          val pos = namePos.withSpan(namePos.span.withStart(parent.span.start))
          // record info and insert call to Invoker.invoked
          val statementId = recordStatement(parent, pos, false)
          insertInvokeCall(body, pos, statementId)

    /** Returns the tree, prepended by a call to Invoker.invoked */
    private def insertInvokeCall(tree: Tree, pos: SourcePosition, statementId: Int)(using Context): Tree =
      val callSpan = syntheticSpan(pos)
      Block(invokeCall(statementId, callSpan) :: Nil, tree).withSpan(callSpan.union(tree.span))

    /** Generates Invoker.invoked(id, DIR) */
    private def invokeCall(id: Int, span: Span)(using Context): Tree =
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
    private def needsLift(tree: Apply)(using Context): Boolean =
      def isShortCircuitedOp(sym: Symbol) =
        sym == defn.Boolean_&& || sym == defn.Boolean_||

      def isUnliftableFun(fun: Tree) =
        /*
         * We don't want to lift a || getB(), to avoid calling getB if a is true.
         * Same idea with a && getB(): if a is false, getB shouldn't be called.
         *
         * On top of that, the `s`, `f` and `raw` string interpolators are special-cased
         * by the compiler and will disappear in phase StringInterpolatorOpt, therefore
         * they shouldn't be lifted.
         */
        val sym = fun.symbol
        sym.exists && (isShortCircuitedOp(sym) || StringInterpolatorOpt.isCompilerIntrinsic(sym))
      end

      val fun = tree.fun
      val nestedApplyNeedsLift = fun match
        case a: Apply => needsLift(a)
        case _ => false

      nestedApplyNeedsLift ||
      !isUnliftableFun(fun) && !tree.args.isEmpty && !tree.args.forall(LiftCoverage.noLift)

    /** Check if the body of a DefDef can be instrumented with instrumentBody. */
    private def canInstrumentDefDef(tree: DefDef)(using Context): Boolean =
      // No need to force the instrumentation of synthetic definitions
      // (it would work, but it looks better without).
      !tree.symbol.isOneOf(Accessor | Synthetic | Artifact) &&
      !tree.rhs.isEmpty

    /** Check if an Apply can be instrumented. Prevents this phase from generating incorrect code. */
    private def canInstrumentApply(tree: Apply)(using Context): Boolean =
      val sym = tree.symbol
      !sym.isOneOf(Synthetic | Artifact) && // no need to instrument synthetic apply
      !isCompilerIntrinsicMethod(sym) &&
      (tree.typeOpt match
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
          /* def f(a: Ta)(b: Tb)
             f(a)(b)

             Here, f(a)(b) cannot be rewritten to {invoked();f(a)}(b)
          */
          false
        case _ =>
          true
      )

    /** Is this the symbol of a parameterless method that we can instrument?
      * Note: it is crucial that `asInstanceOf` and `isInstanceOf`, among others,
      * do NOT get instrumented, because that would generate invalid code and crash
      * in post-erasure checking.
      */
    private def canInstrumentParameterless(sym: Symbol)(using Context): Boolean =
      sym.is(Method, butNot = Synthetic | Artifact) &&
      sym.info.isParameterless &&
      !isCompilerIntrinsicMethod(sym)

    /** Does sym refer to a "compiler intrinsic" method, which only exist during compilation,
      * like Any.isInstanceOf?
      * If this returns true, the call souldn't be instrumented.
      */
    private def isCompilerIntrinsicMethod(sym: Symbol)(using Context): Boolean =
      val owner = sym.maybeOwner
      owner.exists && (
        owner.eq(defn.AnyClass) ||
        owner.isPrimitiveValueClass ||
        owner.maybeOwner == defn.CompiletimePackageClass
      )

object InstrumentCoverage:
  val name: String = "instrumentCoverage"
  val description: String = "instrument code for coverage checking"

  /** Extractor object for trees produced by `insertInvokeCall`. */
  object InstrumentedBlock:
    private def isInvokedCall(app: Apply)(using Context): Boolean =
      app.span.isSynthetic && app.symbol == defn.InvokedMethodRef.symbol

    def unapply(t: Tree)(using Context): Option[(Apply, Tree)] =
      t match
        case Block((app: Apply) :: Nil, expr) if isInvokedCall(app) =>
          Some((app, expr))
        case _ =>
          None
