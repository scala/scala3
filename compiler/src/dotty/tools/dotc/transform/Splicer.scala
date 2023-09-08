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

import dotty.tools.dotc.quoted.Interpreter

import scala.util.control.NonFatal
import dotty.tools.dotc.util.SrcPos

import scala.reflect.ClassTag

import dotty.tools.dotc.quoted.{PickledQuotes, QuoteUtils}

import scala.quoted.Quotes
import scala.quoted.runtime.impl._
import dotty.tools.dotc.core.NameKinds

/** Utility class to splice quoted expressions */
object Splicer {
  import tpd.*
  import Interpreter.*

  /** Splice the Tree for a Quoted expression. `${'{xyz}}` becomes `xyz`
   *  and for `$xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is returned as a `Tree`
   *
   *  See: `Staging`
   */
  def splice(tree: Tree, splicePos: SrcPos, spliceExpansionPos: SrcPos, classLoader: ClassLoader)(using Context): Tree = tree match {
    case Quote(quotedTree, Nil) => quotedTree
    case _ =>
      val macroOwner = newSymbol(ctx.owner, nme.MACROkw, Macro | Synthetic, defn.AnyType, coord = tree.span)
      try
        val sliceContext = SpliceScope.contextWithNewSpliceScope(splicePos.sourcePos).withOwner(macroOwner)
        inContext(sliceContext) {
          val oldContextClassLoader = Thread.currentThread().getContextClassLoader
          Thread.currentThread().setContextClassLoader(classLoader)
          try {
            val interpreter = new SpliceInterpreter(splicePos, classLoader)

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
            report.error("Macro expansion was aborted by the macro without any errors reported. Macros should issue errors to end-users when aborting a macro expansion with StopMacroExpansion.", splicePos)
          // errors have been emitted
          EmptyTree
        case ex: StopInterpretation =>
          report.error(ex.msg, ex.pos)
          ref(defn.Predef_undefined).withType(ErrorType(ex.msg))
        case NonFatal(ex) =>
          val msg =
            em"""Failed to evaluate macro.
                |  Caused by ${ex.getClass}: ${if (ex.getMessage == null) "" else ex.getMessage}
                |    ${ex.getStackTrace.takeWhile(_.getClassName != "dotty.tools.dotc.transform.Splicer$").drop(1).mkString("\n    ")}
              """
          report.error(msg, spliceExpansionPos)
          ref(defn.Predef_undefined).withType(ErrorType(msg))
      }
  }

  /** Checks that no symbol that was generated within the macro expansion has an out of scope reference */
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
          { // symbol was generated within another macro expansion
            isMacroOwner(x) &&
            !ctx.owner.ownersIterator.contains(x)
          }
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
    case Quote(_, Nil) => // ok
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

        case Apply(Select(Quote(body, _), nme.apply), _) =>
          val noSpliceChecker = new TreeTraverser {
            def traverse(tree: Tree)(using Context): Unit = tree match
              case Splice(_) =>
                report.error("Quoted argument of macros may not have splices", tree.srcPos)
              case _ =>
                traverseChildren(tree)
          }
          noSpliceChecker.traverse(body)

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

        case Apply(Select(Quote(quoted, Nil), nme.apply), _) =>
          // OK, canceled and warning emitted

        case Call(fn, args)
            if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package)) ||
               fn.symbol.is(Module) || fn.symbol.isStatic ||
               (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic) =>
          if (fn.symbol.flags.is(Inline))
            report.error("Macro cannot be implemented with an `inline` method", fn.srcPos)
          args.flatten.foreach(checkIfValidArgument)

        case Call(fn, args) if fn.symbol.name.is(NameKinds.InlineAccessorName) =>
          // TODO suggest use of @binaryAPI once we have the annotation
          report.error(
            i"""Macro implementation is not statically accessible.
              |
              |Non-static inline accessor was generated in ${fn.symbol.owner}
              |""".stripMargin, tree.srcPos)
        case _ =>
          report.error(
            """Malformed macro.
              |
              |Expected the splice ${...} to contain a single call to a static method.
              |""".stripMargin, tree.srcPos)
      }

      checkIfValidStaticCall(tree)(using Set.empty)
  }

  /** Is this the dummy owner of a macro expansion */
  def isMacroOwner(sym: Symbol)(using Context): Boolean =
    sym.is(Macro, butNot = Method) && sym.name == nme.MACROkw

  /** Is this the dummy owner of a macro expansion */
  def inMacroExpansion(using Context) =
    ctx.owner.ownersIterator.exists(isMacroOwner)

  /** Tree interpreter that evaluates the tree.
   *  Interpreter is assumed to start at quotation level -1.
   */
  private class SpliceInterpreter(pos: SrcPos, classLoader: ClassLoader)(using Context) extends Interpreter(pos, classLoader) {

    override protected  def interpretTree(tree: Tree)(implicit env: Env): Object = tree match {
      // Interpret level -1 quoted code `'{...}` (assumed without level 0 splices)
      case Apply(Select(Quote(body, _), nme.apply), _) =>
        val body1 = body match {
          case expr: Ident if expr.symbol.isAllOf(InlineByNameProxy) =>
            // inline proxy for by-name parameter
            expr.symbol.defTree.asInstanceOf[DefDef].rhs
          case tree: Inlined if tree.inlinedFromOuterScope => tree.expansion
          case _ => body
        }
        new ExprImpl(Inlined(EmptyTree, Nil, QuoteUtils.changeOwnerOfTree(body1, ctx.owner)).withSpan(body1.span), SpliceScope.getCurrent)

      // Interpret level -1 `Type.of[T]`
      case Apply(TypeApply(fn, quoted :: Nil), _) if fn.symbol == defn.QuotedTypeModule_of =>
        new TypeImpl(QuoteUtils.changeOwnerOfTree(quoted, ctx.owner), SpliceScope.getCurrent)

      case _ =>
        super.interpretTree(tree)
    }
  }
}
