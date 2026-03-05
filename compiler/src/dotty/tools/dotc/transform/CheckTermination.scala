package dotty.tools.dotc
package transform

import ast.tpd
import core.Contexts.{ctx, Context}
import core.Decorators.*
import core.Flags.*
import core.StdNames.nme
import core.Symbols.*
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

      val canDecrease = params.map(_.info.typeSymbol).forall(canTypeDecrease)
      if !canDecrease then
        report.error(
          // This should be more precise.
          s"${method.name} has an argument with a type that is mutable or has overridden unapply",
          method.srcPos
        )
      else
        val checker = new TerminationChecker(method, params)
        checker.traverse(tree.rhs)
    }

    tree
  }

  private def canTypeDecrease(tpeSym: Symbol)(using Context): Boolean = {
    def canCaseDecrease(classSym: ClassSymbol): Boolean = {
      val unapplyMethod = classSym.companionClass.info.member(nme.unapply).symbol
      val unapplySynthetic = unapplyMethod.exists && unapplyMethod.is(Synthetic)
      unapplySynthetic && {
        val fields = classSym.paramAccessors
        val constructorParams = classSym.primaryConstructor.paramSymss.flatten
        fields.forall(sym => sym.isStableMember && constructorParams.exists(_.name == sym.name))
      }
    }

    if tpeSym.is(Sealed) then
      val children = tpeSym.children
      children.nonEmpty && children.forall(canTypeDecrease)
    else
      tpeSym.is(CaseVal) || (tpeSym.is(Case) && canCaseDecrease(tpeSym.asClass))
  }

  private class TerminationChecker(method: Symbol, params: List[Symbol])(using Context) extends TreeTraverser {

    enum Size:
      case Smaller, Same, Unknown

    private var smallerThan = Map.empty[Symbol, Symbol]

    override def traverse(tree: Tree)(using Context): Unit =
      tree match {
        case tree @ Apply(fun, args) if fun.symbol == method =>
          val argsSizes = args.map(argSize)
          val hasSmaller = argsSizes.contains(Size.Smaller)
          val hasUnknown = argsSizes.contains(Size.Unknown)
          if !hasSmaller || hasUnknown then {
            report.error(
              s"Recursive call to ${method.name} may not terminate",
              tree.srcPos
            )
          }

          traverseChildren(tree)

        case tree @ Match(selector, cases) =>
          // Supposing selector is an Ident for now.
          // traverse(selector)

          cases.foreach(cse =>
            val savedMap = smallerThan
            mapSymbols(selector.symbol, cse.pat)
            traverse(cse.guard)
            traverse(cse.body)
            smallerThan = savedMap
          )

        case _ => traverseChildren(tree)

      }

    private def argSize(arg: Tree)(using Context): Size = {
      def symbolSize(sym: Symbol): Size =
        if params.contains(sym) then Size.Same
        else
          smallerThan.get(sym) match {
            case Some(largerSym) =>
              symbolSize(largerSym) match {
                case Size.Unknown => Size.Unknown
                case _ => Size.Smaller
              }
            case None => Size.Unknown
          }

      val argSymbol = arg.symbol
      if params.contains(argSymbol) then Size.Same
      else if smallerThan.contains(argSymbol) then symbolSize(argSymbol)
      else
        arg match {
          case Select(qualifier, _) => argSize(qualifier)
          case Apply(fn, _) => argSize(fn)
          case TypeApply(fn, _) => argSize(fn)
          case _ => Size.Unknown
        }
    }

    private def mapSymbols(selector: Symbol, pat: Tree)(using Context): Unit = {
      def bindingSymbols(tree: Tree): Set[Symbol] = {
        var bindings = Set.empty[Symbol]
        val traverser = new TreeTraverser {
          override def traverse(t: Tree)(using Context): Unit =
            t match {
              case b @ Bind(_, _) => bindings += b.symbol
              case _ => ()
            }
            traverseChildren(t)
        }
        tree match {
          case Bind(_, tree) => // ignore the `case l @ ...`
            traverser.traverse(tree)
          case _ => traverser.traverse(tree)
        }
        bindings
      }

      bindingSymbols(pat).foreach(bind => smallerThan += (bind -> selector))
    }

  }
}

object CheckTermination:
  val name = "check-termination"
  val description = "check if annotated functions terminate"

