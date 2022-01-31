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

  override def phaseName: String = InlinePatterns.name

  override def description: String = InlinePatterns.description

  // This phase needs to run after because it need to transform trees that are generated
  // by the pattern matcher but are still not visible in that group of phases.
  override def runsAfterGroupsOf: Set[String] = Set(PatternMatcher.name)

  override def transformApply(app: Apply)(using Context): Tree =
    if app.symbol.name.isUnapplyName && !app.tpe.isInstanceOf[MethodicType] then
      app match
        case App(Select(fn, name), argss) =>
          val app1 = betaReduce(app, fn, name, argss.flatten)
          if app1 ne app then report.log(i"beta reduce $app -> $app1")
          app1
        case _ =>
          app
    else app

  private object App:
    def unapply(app: Tree): (Tree, List[List[Tree]]) =
      app match
        case Apply(App(fn, argss), args) => (fn, argss :+ args)
        case _ => (app, Nil)

  private def betaReduce(tree: Apply, fn: Tree, name: Name, args: List[Tree])(using Context): Tree =
    fn match
      case Block(TypeDef(_, template: Template) :: Nil, Apply(Select(New(_),_), Nil)) if template.constr.rhs.isEmpty =>
        template.body match
          case List(ddef @ DefDef(`name`, _, _, _)) => BetaReduce(ddef, args)
          case _ => tree
      case _ => tree

object InlinePatterns:
  val name: String = "inlinePatterns"
  val description: String = "remove placeholders of inlined patterns"

