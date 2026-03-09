package dotty.tools.dotc
package transform

import util.SrcPos
import ast.tpd
import core.Contexts.{ctx, Context}
import core.Decorators.*
import core.Flags.*
import core.StdNames.nme
import core.Symbols.*
import core.Types.*
import MegaPhase.MiniPhase

class CheckTermination extends MiniPhase {
  import tpd.*

  override def phaseName: String = CheckTermination.name

  override def description: String = CheckTermination.description

  override def isEnabled(using Context): Boolean = ctx.settings.YcheckTermination.value

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val method = tree.symbol
    val mandatory = method.hasAnnotation(defn.TerminationAnnot)

    if mandatory && !method.is(Deferred) then {
      val params = tree.termParamss.flatten.map(_.symbol)
      val checker = new TerminationChecker(method, params)
      checker.traverse(tree.rhs)
    }

    tree
  }

  private class TerminationChecker(startMethod: Symbol, startParams: List[Symbol])(using Context)
      extends TreeTraverser {

    enum Size:
      case Smaller, Same, Unknown

    private var sizeMap = Map.empty[Symbol, (Symbol, Size)]

    private var callStack = List[(method: Symbol, params: List[Symbol])]((startMethod, startParams))

    override def traverse(tree: Tree)(using Context): Unit = {
      tree match {
        case tree: DefDef => () // Don't traverse inner function definitions.

        case tree @ Apply(fn, args) =>
          val argSymbols = args.map(_.symbol)

          callStack.find(_.method == fn.symbol) match {
            case Some((method, params)) =>
              val (allArgs, allParams) = fn match {
                case Select(qualifier, _) =>
                  val thisSymbol = method.enclosingClass.thisType.typeSymbol
                  (qualifier.symbol :: argSymbols, thisSymbol :: params)
                case _ => (argSymbols, params)
              }
              if !areSmaller(allArgs, allParams) then {
                report.error(
                  s"${startMethod.name} may not terminate due to (mutually) recursive call.",
                  tree.srcPos
                )
              }

            case None =>
              val methodSymbol = fn.symbol
              methodSymbol.defTree match {
                case tree: DefDef =>
                  val params = methodSymbol.paramSymss.filter(!_.exists(_.isTypeParam)).head
                  callStack = (methodSymbol, params) :: callStack
                  val savedMap = sizeMap

                  params.zip(args).foreach((param, arg) => sizeMap += param -> (arg.symbol, Size.Same))
                  traverse(tree.rhs)

                  sizeMap = savedMap
                  callStack = callStack.tail
                case _ => ()
              }
          }
          traverseChildren(tree)

        case tree @ Match(selector, cases) =>
          val syntheticUnapply = isUnapplySynthetic(selector)
          cases.foreach(cse =>
            val savedMap = sizeMap
            if syntheticUnapply then { mapSymbols(selector.symbol, cse.pat) }
            traverse(cse.guard)
            traverse(cse.body)
            sizeMap = savedMap
          )

        case tree: ValDef =>
          sizeMap += tree.symbol -> (tree.rhs.symbol -> Size.Same)
          traverseChildren(tree)

        case _ => traverseChildren(tree)
      }
    }

    private def areSmaller(args: List[Symbol], params: List[Symbol])(using Context): Boolean = {
      def compareSize(arg: Symbol, param: Symbol, decreased: Boolean = false): Size =
        if arg == param then
          if decreased then Size.Smaller else Size.Same
        else
          sizeMap.get(arg) match {
            case None => Size.Unknown
            case Some((sym, size)) =>
              size match {
                case Size.Smaller => compareSize(sym, param, true)
                case Size.Same => compareSize(sym, param, decreased)
                case Size.Unknown => Size.Unknown
              }
          }

      def isLexicoDecreasing(argsParams: List[(Symbol, Symbol)]): Boolean =
        argsParams match {
          case (arg, param) :: tail =>
            compareSize(arg, param) match {
              case Size.Smaller => typeWellFounded(param.info.typeSymbol, arg.srcPos)
              case Size.Same => isLexicoDecreasing(tail)
              case Size.Unknown => false
            }
          case Nil => false
        }

      isLexicoDecreasing(args.zip(params))
    }

    private def isUnapplySynthetic(selector: Tree)(using Context): Boolean = {
      def check(tpeSym: Symbol): Boolean =
        val res = tpeSym.is(Case) && tpeSym.isClass && {
          val unapplyMethod = tpeSym.asClass.companionClass.info.member(nme.unapply).symbol
          unapplyMethod.exists && unapplyMethod.is(Synthetic)
        }
        if !res then {
          report.warning(
            s"${selector.symbol.name} may not structurally decrease because ${tpeSym.name} unapply method is overridden or undefined.",
            selector.srcPos
          )
        }
        res

      val tpeSym = selector.symbol.info.typeSymbol
      if tpeSym.is(Sealed) then tpeSym.children.forall(c => c.is(CaseVal) || check(c))
      else check(tpeSym)
    }

    private def typeWellFounded(tpeSym: Symbol, pos: SrcPos)(using Context): Boolean = {
      def caseClassCheck(classSym: ClassSymbol): Boolean =
        val fields = classSym.paramAccessors
        val constructorParams = classSym.primaryConstructor.paramSymss.filter(!_.exists(_.isTypeParam)).head
        fields.forall(symbol => symbol.isStableMember && constructorParams.exists(_.name == symbol.name))

      val res = {
        if tpeSym.is(Sealed) then tpeSym.children.forall(typeWellFounded(_, pos))
        else tpeSym.is(CaseVal) || (tpeSym.is(Case) && tpeSym.isClass && caseClassCheck(tpeSym.asClass))
      }

      if !res then {
        report.warning(
          s"Argument of type ${tpeSym.name} decreases but the type is not well-founded.",
          pos
        )
      }

      res
    }

    private def mapSymbols(selector: Symbol, pat: Tree)(using Context): Unit = {
      val traverser = new TreeTraverser {
        override def traverse(tree: Tree)(using Context): Unit =
          tree match {
            case bind @ Bind(_, _) =>
              sizeMap += bind.symbol -> (selector, Size.Smaller)
            case _ => ()
          }
          traverseChildren(tree)
      }

      pat match {
        case bind @ Bind(_, tree) =>
          sizeMap += bind.symbol -> (selector, Size.Same)
          traverser.traverse(tree)
        case _ => traverser.traverse(pat)
      }
    }

  }
}

object CheckTermination:
  val name = "check-termination"
  val description = "check if annotated functions terminate"
