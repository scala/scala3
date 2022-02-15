package dotty.tools.dotc
package transform

import scala.language.unsafeNulls

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.{InvocationTargetException, Method => JLRMethod}

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds.FlatName
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Denotations.staticRef
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Constants.Constant

import scala.util.control.NonFatal
import dotty.tools.dotc.util.SrcPos
import dotty.tools.repl.AbstractFileClassLoader

import scala.reflect.ClassTag

import dotty.tools.dotc.quoted.{PickledQuotes, QuoteUtils}

import scala.quoted.Quotes
import scala.quoted.runtime.impl._

/** Utility class to splice quoted expressions */
object Splicer {
  import tpd._

  /** Splice the Tree for a Quoted expression. `${'{xyz}}` becomes `xyz`
   *  and for `$xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is returned as a `Tree`
   *
   *  See: `Staging`
   */
  def splice(tree: Tree, splicePos: SrcPos, spliceExpansionPos: SrcPos, classLoader: ClassLoader)(using Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case _ =>
      val macroOwner = newSymbol(ctx.owner, nme.MACROkw, Macro | Synthetic, defn.AnyType, coord = tree.span)
      try
        val sliceContext = SpliceScope.contextWithNewSpliceScope(splicePos.sourcePos).withOwner(macroOwner)
        inContext(sliceContext) {
          val oldContextClassLoader = Thread.currentThread().getContextClassLoader
          Thread.currentThread().setContextClassLoader(classLoader)
          try {
            val interpreter = new InterpreterSplicer(splicePos, classLoader)

            // Some parts of the macro are evaluated during the unpickling performed in quotedExprToTree
            val interpretedExpr = interpreter.interpret[Quotes => scala.quoted.Expr[Any]](tree)
            val interpretedTree = interpretedExpr.fold(tree)(macroClosure => PickledQuotes.quotedExprToTree(macroClosure(QuotesImpl())))

            checkEscapedVariables(interpretedTree, macroOwner)
          } finally {
            Thread.currentThread().setContextClassLoader(oldContextClassLoader)
          }
        }.changeOwner(macroOwner, ctx.owner)
      catch {
        case ex: CompilationUnit.SuspendException =>
          throw ex
        case ex: scala.quoted.runtime.StopMacroExpansion =>
          if !ctx.reporter.hasErrors then
            report.error("Macro expansion was aborted by the macro without any errors reported. Macros should issue errors to end-users to facilitate debugging when aborting a macro expansion.", splicePos)
          // errors have been emitted
          EmptyTree
        case ex: StopInterpretation =>
          report.error(ex.msg, ex.pos)
          ref(defn.Predef_undefined).withType(ErrorType(ex.msg))
        case NonFatal(ex) =>
          val msg =
            s"""Failed to evaluate macro.
               |  Caused by ${ex.getClass}: ${if (ex.getMessage == null) "" else ex.getMessage}
               |    ${ex.getStackTrace.takeWhile(_.getClassName != "dotty.tools.dotc.transform.Splicer$").drop(1).mkString("\n    ")}
             """.stripMargin
          report.error(msg, spliceExpansionPos)
          ref(defn.Predef_undefined).withType(ErrorType(msg))
      }
  }

  /** Checks that no symbol that whas generated within the macro expansion has an out of scope reference */
  def checkEscapedVariables(tree: Tree, expansionOwner: Symbol)(using Context): tree.type =
    new TreeTraverser {
      private[this] var locals = Set.empty[Symbol]
      private def markSymbol(sym: Symbol)(using Context): Unit =
          locals = locals + sym
      private def markDef(tree: Tree)(using Context): Unit = tree match {
        case tree: DefTree => markSymbol(tree.symbol)
        case _ =>
      }
      def traverse(tree: Tree)(using Context): Unit =
        def traverseOver(lastEntered: Set[Symbol]) =
          try traverseChildren(tree)
          finally locals = lastEntered
        tree match
          case tree: Ident if isEscapedVariable(tree.symbol) =>
            val sym = tree.symbol
            report.error(em"While expanding a macro, a reference to $sym was used outside the scope where it was defined", tree.srcPos)
          case Block(stats, _) =>
            val last = locals
            stats.foreach(markDef)
            traverseOver(last)
          case CaseDef(pat, guard, body) =>
            val last = locals
            tpd.patVars(pat).foreach(markSymbol)
            traverseOver(last)
          case _ =>
            markDef(tree)
            traverseChildren(tree)
      private def isEscapedVariable(sym: Symbol)(using Context): Boolean =
        sym.exists && !sym.is(Package)
        && sym.owner.ownersIterator.exists(x =>
          x == expansionOwner || // symbol was generated within this macro expansion
          x.is(Macro, butNot = Method) && x.name == nme.MACROkw // symbol was generated within another macro expansion
        )
        && !locals.contains(sym) // symbol is not in current scope
    }.traverse(tree)
    tree


  /** Check that the Tree can be spliced. `${'{xyz}}` becomes `xyz`
    *  and for `$xyz` the tree of `xyz` is interpreted for which the
    *  resulting expression is returned as a `Tree`
    *
    *  See: `Staging`
    */
  def checkValidMacroBody(tree: Tree)(using Context): Unit = tree match {
    case Quoted(_) => // ok
    case _ =>
      type Env = Set[Symbol]

      def checkValidStat(tree: Tree)(using Env): Env = tree match {
        case tree: ValDef if tree.symbol.is(Synthetic) =>
          // Check val from `foo(j = x, i = y)` which it is expanded to
          // `val j$1 = x; val i$1 = y; foo(i = i$1, j = j$1)`
          checkIfValidArgument(tree.rhs)
          summon[Env] + tree.symbol
        case _ =>
          report.error("Macro should not have statements", tree.srcPos)
          summon[Env]
      }

      def checkIfValidArgument(tree: Tree)(using Env): Unit = tree match {
        case Block(Nil, expr) => checkIfValidArgument(expr)
        case Typed(expr, _) => checkIfValidArgument(expr)

        case Apply(Select(Apply(fn, quoted :: Nil), nme.apply), _) if fn.symbol == defn.QuotedRuntime_exprQuote =>
          val noSpliceChecker = new TreeTraverser {
            def traverse(tree: Tree)(using Context): Unit = tree match
              case Spliced(_) =>
                report.error("Quoted argument of macros may not have splices", tree.srcPos)
              case _ =>
                traverseChildren(tree)
          }
          noSpliceChecker.traverse(quoted)

        case Apply(TypeApply(fn, List(quoted)), _)if fn.symbol == defn.QuotedTypeModule_of =>
          // OK

        case Literal(Constant(value)) =>
          // OK

        case NamedArg(_, arg) =>
          checkIfValidArgument(arg)

        case SeqLiteral(elems, _) =>
          elems.foreach(checkIfValidArgument)

        case tree: Ident if summon[Env].contains(tree.symbol) || tree.symbol.is(Inline, butNot = Method) =>
          // OK

        case _ =>
          val extra = if tree.span.isZeroExtent then ": " + tree.show else ""
          report.error(
            s"""Malformed macro parameter$extra
              |
              |Parameters may only be:
              | * Quoted parameters or fields
              | * Literal values of primitive types
              | * References to `inline val`s
              |""".stripMargin, tree.srcPos)
      }

      def checkIfValidStaticCall(tree: Tree)(using Env): Unit = tree match {
        case closureDef(ddef @ DefDef(_, ValDefs(ev :: Nil) :: Nil, _, _)) if ddef.symbol.info.isContextualMethod =>
          checkIfValidStaticCall(ddef.rhs)(using summon[Env] + ev.symbol)

        case Block(stats, expr) =>
          val newEnv = stats.foldLeft(summon[Env])((env, stat) => checkValidStat(stat)(using env))
          checkIfValidStaticCall(expr)(using newEnv)

        case Typed(expr, _) =>
          checkIfValidStaticCall(expr)

        case Apply(Select(Apply(fn, quoted :: Nil), nme.apply), _) if fn.symbol == defn.QuotedRuntime_exprQuote =>
          // OK, canceled and warning emitted

        case Call(fn, args)
            if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package)) ||
               fn.symbol.is(Module) || fn.symbol.isStatic ||
               (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic) =>
          if (fn.symbol.flags.is(Inline))
            report.error("Macro cannot be implemented with an `inline` method", fn.srcPos)
          args.flatten.foreach(checkIfValidArgument)

        case _ =>
          report.error(
            """Malformed macro.
              |
              |Expected the splice ${...} to contain a single call to a static method.
              |""".stripMargin, tree.srcPos)
      }

      checkIfValidStaticCall(tree)(using Set.empty)
  }

  /** Tree interpreter that evaluates the tree */
  private class InterpreterSplicer(pos: SrcPos, classLoader: ClassLoader)(using Context) extends Interpreter(pos, classLoader) {

    override def interpretTree(tree: Tree)(implicit env: Env): Object = tree match {
      case Apply(Select(Apply(TypeApply(fn, _), quoted :: Nil), nme.apply), _) if fn.symbol == defn.QuotedRuntime_exprQuote =>
        val quoted1 = quoted match {
          case quoted: Ident if quoted.symbol.isAllOf(InlineByNameProxy) =>
            // inline proxy for by-name parameter
            quoted.symbol.defTree.asInstanceOf[DefDef].rhs
          case Inlined(EmptyTree, _, quoted) => quoted
          case _ => quoted
        }
        interpretQuote(quoted1)

      case Apply(TypeApply(fn, quoted :: Nil), _) if fn.symbol == defn.QuotedTypeModule_of =>
        interpretTypeQuote(quoted)

      case _ => super.interpretTree(tree)
    }

    private def interpretQuote(tree: Tree)(implicit env: Env): Object =
      new ExprImpl(Inlined(EmptyTree, Nil, QuoteUtils.changeOwnerOfTree(tree, ctx.owner)).withSpan(tree.span), SpliceScope.getCurrent)

    private def interpretTypeQuote(tree: Tree)(implicit env: Env): Object =
      new TypeImpl(QuoteUtils.changeOwnerOfTree(tree, ctx.owner), SpliceScope.getCurrent)
  }
}
