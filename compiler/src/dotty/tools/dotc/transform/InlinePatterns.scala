package dotty.tools
package dotc
package transform

import core._
import MegaPhase._
import Symbols._, Contexts._, Types._, Decorators._
import StdNames.nme
import NameOps._
import Names._
import ast.Trees._
import ast.TreeTypeMap

/** Rewrite an application
 *
 *    {new { def unapply(x0: X0)(x1: X1,..., xn: Xn) = b }}.unapply(y0)(y1, ..., yn)
 *
 *  where
 *
 *    - the method is `unapply` or `unapplySeq`
 *    - the method does not have type parameters
 *
 *  to
 *
 *    [xi := yi]b
 *
 *  This removes placeholders added by inline `unapply`/`unapplySeq` patterns.
 */
class InlinePatterns extends MiniPhase:
  import ast.tpd._

  def phaseName: String = "inlinePatterns"

  override def runsAfterGroupsOf: Set[String] = Set(PatternMatcher.name)

  override def transformApply(app: Apply)(using ctx: Context): Tree =
    if app.symbol.name.isUnapplyName && !app.tpe.isInstanceOf[MethodicType] then
      app match
        case App(Select(fn, name), argss) =>
          val app1 = betaReduce(app, fn, name, argss.flatten)
          if app1 ne app then ctx.log(i"beta reduce $app -> $app1")
          app1
        case _ =>
          app
    else app

  private object App:
    def unapply(app: Tree): (Tree, List[List[Tree]]) =
      app match
        case Apply(App(fn, argss), args) => (fn, argss :+ args)
        case _ => (app, Nil)

  private def betaReduce(tree: Apply, fn: Tree, name: Name, args: List[Tree])(using ctx: Context): Tree =
    fn match
      case Block(Nil, expr) => betaReduce(tree, expr, name, args)
      case Block(TypeDef(_, template: Template) :: Nil, Apply(Select(New(_),_), Nil)) if template.constr.rhs.isEmpty =>
        template.body match
          case List(ddef @ DefDef(`name`, _, _, _, _)) =>
            val bindings = List.newBuilder[ValDef]
            val vparams = ddef.vparamss.flatten
            val argSyms =
              for (arg, param) <- args.zip(vparams) yield
                arg.tpe.dealias match
                  case ref @ TermRef(NoPrefix, _) if isPurePath(arg) =>
                    ref.symbol
                  case _ =>
                    val binding = SyntheticValDef(param.name, arg)
                    bindings += binding
                    binding.symbol
            seq(
              bindings.result(),
              TreeTypeMap(
                oldOwners = ddef.symbol :: Nil,
                newOwners = ctx.owner :: Nil,
                substFrom = vparams.map(_.symbol),
                substTo = argSyms).transform(ddef.rhs)
            )

          case _ => tree
      case _ => tree
