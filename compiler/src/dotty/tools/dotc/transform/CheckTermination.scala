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
      val checker = new TerminationChecker(method)
      checker.traverse(tree.rhs)
    }

    tree
  }

  private class TerminationChecker(startMethod: Symbol)(using Context)
      extends TreeTraverser {

    enum Size:
      case Smaller, Same, Unknown

    private var sizeMap = Map.empty[Symbol, (Symbol | Tree, Size)]

    private var callStack = List[Symbol](startMethod)

    override def traverse(tree: Tree)(using Context): Unit = {
      tree match {
        case tree: DefDef => () // Don't traverse inner function definitions.

        case tree: WhileDo =>
          report.warning(
            "The termination checker does not support while loops.",
            tree.srcPos
          )

        case tree @ Apply(fn, _) if fn.symbol == defn.assumeTerminatesMethod =>
          () // Don't traverse, as we assume they terminate.

        case tree @ Apply(fn, _) if fn.symbol.hasAnnotation(defn.AssumeTerminatesAnnot) =>
          traverseChildren(tree)

        case tree: Apply =>
          val (fn, args) = peelApplies(tree)

          callStack.find(_ == fn.symbol) match {
            case Some(methodSymbol) =>
              val params = getMethodParams(methodSymbol)
              if !areSmaller(args, params, methodSymbol) then {
                report.error(
                  s"${startMethod.name} may not terminate due to (mutually) recursive call.",
                  tree.srcPos
                )
              }

            case None =>
              val methodSymbol = getMethodSymbol(fn)
              methodSymbol.defTree match {
                case defTree: DefDef if !defTree.rhs.isEmpty || methodSymbol.isConstructor =>
                  val params = getMethodParams(methodSymbol)
                  callStack = methodSymbol :: callStack
                  val savedMap = sizeMap

                  params.zip(args).foreach((param, arg) => sizeMap += param -> (arg, Size.Same))
                  traverse(defTree.rhs)

                  sizeMap = savedMap
                  callStack = callStack.tail
                case _ =>
                  if methodSymbol.isRealMethod then
                    report.warning(s"Method ${methodSymbol.name} has an empty tree.", tree.srcPos)
              }
          }
          traverse(fn)
          args.foreach{
            case tree: Tree => traverse(tree)
            case _ => ()
          }

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
          tree.rhs match {
            case Select(qualifier, _) =>
              peelSelects(tree.rhs) match {
                case Some((symbol, isSmaller)) =>
                  sizeMap += tree.symbol -> (symbol -> (if isSmaller then Size.Smaller else Size.Same))
                case _ => sizeMap += tree.symbol -> (tree.rhs -> Size.Same)
              }
            case _ => sizeMap += tree.symbol -> (tree.rhs -> Size.Same)
          }
          traverseChildren(tree)

        case _ => traverseChildren(tree)
      }
    }

    private def getMethodParams(methodSymbol: Symbol)(using Context): List[Symbol] = {
      val thisSymbol = methodSymbol.enclosingClass.thisType.typeSymbol
      thisSymbol :: methodSymbol.paramSymss.filter(!_.exists(_.isTypeParam)).flatten
    }

    private def getMethodSymbol(fn: Tree)(using Context): Symbol = {
      val symbol = fn match {
        case Select(qualifier, _) => qualifier.symbol
        case _ => fn.symbol
      }
      sizeMap.get(symbol) match {
        case Some((op, _)) =>
          op match {
            case Block(tree :: _, Closure(_, _, _)) => tree.symbol
            case op: Tree => getMethodSymbol(op)
            case _ => fn.symbol
          }
        case None => fn.symbol
      }
    }

    private def peelApplies(tree: Apply)(using Context): (Tree, List[Symbol | Tree]) = {
      def loop(tree: Tree, acc: List[Tree] = Nil): (Tree, List[Tree]) =
        tree match {
          case Apply(fn, args) => loop(fn, args ++ acc)
          case TypeApply(fn, _) => loop(fn, acc)
          case _ => (tree, acc)
        }

      val (fn, args) = loop(tree)
      (fn,
        (fn match {
          case Select(qualifier, _) => qualifier.symbol
          case TypeApply(Select(qualifier, _), _) => qualifier.symbol
          case _ => fn.symbol.enclosingClass.thisType.typeSymbol
        }) :: args
      )
    }

    private def getMeasure(tree: DefDef)(using Context) = {
      val candidates = tree.tpt.tpe.getAnnotation(defn.DecreasesByAnnot) match {
        case Some(annot) =>
          annot.argument(0) match {
            case Some(Apply(_, args)) => args.map(_.symbol)
            case Some(arg @ Ident(_)) => List(arg.symbol)
            case _ => Nil
          }
        case _ => Nil
      }
      candidates.filter(getMethodParams(tree.symbol).contains)
    }

    private def peelSelects(tree: Tree)(using Context): Option[(Symbol, Boolean)] = {
      def isQualifierSmaller(qualifier: Tree): Option[(Symbol, Boolean)] = {
        peelSelects(qualifier) match {
          case Some(symbol, _) =>
            val tpeSym = symbol.info.typeSymbol
            val res = tree.symbol.hasAnnotation(defn.AssumeDecreasesAnnot) || {
              tpeSym.is(Case) && tpeSym.isClass && {
                val constructorParams = tpeSym.asClass.primaryConstructor
                  .paramSymss.filter(!_.exists(_.isTypeParam)).head
                  constructorParams.exists(_.name == tree.symbol.name)
              }
            }
            if res then Some((symbol, res)) else None
          case None => None
        }
      }
      tree match {
        case Ident(_) => Some((tree.symbol, false))
        case Select(qualifier, _) => isQualifierSmaller(qualifier)
        case Apply(Select(qualifier, _), _) => isQualifierSmaller(qualifier)
        case _ => None
      }
    }

    private def areSmaller(args: List[Symbol | Tree], params: List[Symbol], methodSymbol: Symbol)(using Context): Boolean = {
      def compareSize(arg: Symbol, param: Symbol, decreased: Boolean = false): Size =
        if arg == param then
          if decreased then Size.Smaller else Size.Same
        else
          sizeMap.get(arg) match {
            case None => Size.Unknown
            case Some((result, size)) =>
              val (symbol, isSmaller) = result match {
                case symbol: Symbol => (symbol, false)
                case tree: Tree =>
                  peelSelects(tree) match {
                    case Some(res) => res
                    case None => (tree.symbol, false)
                  }
              }
              size match {
                case Size.Smaller => compareSize(symbol, param, true)
                case Size.Same => compareSize(symbol, param, isSmaller || decreased)
                case Size.Unknown => Size.Unknown
              }
          }

      def isLexicoDecreasing(argsParams: List[(Symbol | Tree, Symbol)]): Boolean =
        argsParams match {
          case (arg, param) :: tail =>
            val (argSymbol, decreased) = arg match {
              case sym: Symbol => (sym, false)
              case tree: Tree =>
                peelSelects(tree) match {
                  case Some(res) => res
                  case None => (tree.symbol, false)
                }
            }
            compareSize(argSymbol, param, decreased) match {
              case Size.Smaller => typeWellFounded(param.info.typeSymbol, argSymbol.srcPos)
              case Size.Same => isLexicoDecreasing(tail)
              case Size.Unknown => false
            }
          case Nil => false
        }

      val measure = methodSymbol.defTree match {
        case tree: DefDef => getMeasure(tree)
        case _ => Nil
      }
      val measureMap = measure.zipWithIndex.toMap
      val orderdArgsParams = args.zip(params).sortBy((_, param) =>
          measureMap.getOrElse(param, Int.MaxValue)
      )
      isLexicoDecreasing(orderdArgsParams)
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

      val res = tpeSym.hasAnnotation(defn.AssumeWellFoundedAnnot) || {
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
