package dotty.tools
package dotc
package transform

import core.*
import MegaPhase.*
import Symbols.*, Contexts.*, Types.*, Decorators.*
import NameOps.*
import Names.*

import scala.collection.mutable.ListBuffer

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
  import ast.tpd.*

  override def phaseName: String = InlinePatterns.name

  override def description: String = InlinePatterns.description

  // This phase needs to run after because it need to transform trees that are generated
  // by the pattern matcher but are still not visible in that group of phases.
  override def runsAfterGroupsOf: Set[String] = Set(PatternMatcher.name)

  override def transformApply(app: Apply)(using Context): Tree =
    if app.symbol.name.isUnapplyName && !app.tpe.isInstanceOf[MethodicType] then
      app match
        case App(Select(fn, name), argss) =>
          val app1 = betaReduce(app, fn, name, argss)
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

  // TODO merge with BetaReduce.scala
  private def betaReduce(tree: Apply, fn: Tree, name: Name, argss: List[List[Tree]])(using Context): Tree =
    fn match
      case Block(TypeDef(_, template: Template) :: Nil, Apply(Select(New(_),_), Nil)) if template.constr.rhs.isEmpty =>
        template.body match
          case List(ddef @ DefDef(`name`, _, _, _)) =>
            val bindings = new ListBuffer[DefTree]()
            val expansion1 = BetaReduce.reduceApplication(ddef, argss, bindings)
            val bindings1 = bindings.result()
            seq(bindings1, expansion1)
          case _ => tree
      case _ => tree

object InlinePatterns:
  val name: String = "inlinePatterns"
  val description: String = "remove placeholders of inlined patterns"

