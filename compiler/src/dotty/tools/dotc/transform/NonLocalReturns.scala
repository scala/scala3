package dotty.tools.dotc
package transform

import core._
import Contexts._, Symbols._, Types._, Flags._, StdNames._
import MegaPhase._
import NameKinds.NonLocalReturnKeyName
import config.Feature.sourceVersion
import config.SourceVersion._

object NonLocalReturns {
  import ast.tpd._

  val name: String = "nonLocalReturns"
  val description: String = "expand non-local returns"

  def isNonLocalReturn(ret: Return)(using Context): Boolean =
    !ret.from.symbol.is(Label) && (ret.from.symbol != ctx.owner.enclosingMethod || ctx.owner.is(Lazy))
}

/** Implement non-local returns using NonLocalReturnControl exceptions.
 */
class NonLocalReturns extends MiniPhase {

  override def phaseName: String = NonLocalReturns.name

  override def description: String = NonLocalReturns.description

  import NonLocalReturns._
  import ast.tpd._

  override def runsAfter: Set[String] = Set(ElimByName.name)

  private def ensureConforms(tree: Tree, pt: Type)(using Context) =
    if (tree.tpe <:< pt) tree
    else Erasure.Boxing.adaptToType(tree, pt)

  private def nonLocalReturnControl(using Context) = defn.NonLocalReturnControlClass.typeRef

  /** The type of a non-local return expression with given argument type */
  private def nonLocalReturnExceptionType(argtype: Type)(using Context) =
    nonLocalReturnControl.appliedTo(argtype)

  /** A hashmap from method symbols to non-local return keys */
  private val nonLocalReturnKeys = MutableSymbolMap[TermSymbol]()

  /** Return non-local return key for given method */
  private def nonLocalReturnKey(meth: Symbol)(using Context) =
    nonLocalReturnKeys.getOrElseUpdate(meth,
      newSymbol(
        meth, NonLocalReturnKeyName.fresh(), Synthetic, defn.ObjectType, coord = meth.span))

  /** Generate a non-local return throw with given return expression from given method.
   *  I.e. for the method's non-local return key, generate:
   *
   *    throw new NonLocalReturnControl(key, expr)
   *  todo: maybe clone a pre-existing exception instead?
   *  (but what to do about exceptions that miss their targets?)
   */
  private def nonLocalReturnThrow(expr: Tree, meth: Symbol)(using Context) =
    Throw(
      New(
        nonLocalReturnControl,
        ref(nonLocalReturnKey(meth)) :: expr.ensureConforms(defn.ObjectType) :: Nil))

  /** Transform (body, key) to:
   *
   *  {
   *    val key = new Object()
   *    try {
   *      body
   *    } catch {
   *      case ex: NonLocalReturnControl =>
   *        if (ex.key().eq(key)) ex.value().asInstanceOf[T]
   *        else throw ex
   *    }
   *  }
   */
  private def nonLocalReturnTry(body: Tree, key: TermSymbol, meth: Symbol)(using Context) = {
    val keyDef = ValDef(key, New(defn.ObjectType, Nil))
    val ex = newSymbol(meth, nme.ex, Case, nonLocalReturnControl, coord = body.span)
    val pat = BindTyped(ex, nonLocalReturnControl)
    val rhs = If(
        ref(ex).select(nme.key).appliedToNone.select(nme.eq).appliedTo(ref(key)),
        ref(ex).select(nme.value).ensureConforms(meth.info.finalResultType),
        Throw(ref(ex)))
    val catches = CaseDef(pat, EmptyTree, rhs) :: Nil
    val tryCatch = Try(body, catches, EmptyTree)
    Block(keyDef :: Nil, tryCatch)
  }

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    nonLocalReturnKeys.remove(tree.symbol) match
      case key: TermSymbol => cpy.DefDef(tree)(rhs = nonLocalReturnTry(tree.rhs, key, tree.symbol))
      case null => tree

  override def transformReturn(tree: Return)(using Context): Tree =
    if isNonLocalReturn(tree) then
      report.errorOrMigrationWarning(
          "Non local returns are no longer supported; use scala.util.control.NonLocalReturns instead",
          tree.srcPos,
          from = future)
      nonLocalReturnThrow(tree.expr, tree.from.symbol).withSpan(tree.span)
    else tree
}
