package dotty.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._, Constants._
import Flags._
import ast.Trees._
import MegaPhase.MiniPhase
import scala.collection.mutable

/** Translates quoted terms and types to `unpickle` method calls.
 *  Checks that the phase consistency principle (PCP) holds.
 */
class ReifyQuotes extends MacroTransform {
  import ast.tpd._

  override def phaseName: String = "reifyQuotes"

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.containsQuotes) super.run

  protected def newTransformer(implicit ctx: Context): Transformer = new Reifier

  /** Serialize `tree`. Embedded splices are represented as nodes of the form
   *
   *      Select(qual, sym)
   *
   *  where `sym` is either `defn.QuotedExpr_~` or `defn.QuotedType_~`. For any splice,
   *  the `qual` part should not be pickled, since it will be added separately later
   *  as a splice.
   */
  def pickleTree(tree: Tree, isType: Boolean)(implicit ctx: Context): String =
    tree.show // TODO: replace with TASTY

  private class Reifier extends Transformer {

    /** The current staging level */
    private var currentLevel = 0

    /** The splices encountered so far, indexed by staging level */
    private val splicesAtLevel = mutable.ArrayBuffer(new mutable.ListBuffer[Tree])

    // Invariant: -1 <= currentLevel <= splicesAtLevel.length

    /** A map from locally defined symbol's to the staging levels of their definitions */
    private val levelOf = new mutable.HashMap[Symbol, Int]

    /** A stack of entered symbol's, to be unwound after block exit */
    private var enteredSyms: List[Symbol] = Nil

    /** Enter staging level of symbol defined by `tree`, if applicable. */
    def markDef(tree: Tree)(implicit ctx: Context) = tree match {
      case tree: MemberDef if !levelOf.contains(tree.symbol) =>
        levelOf(tree.symbol) = currentLevel
        enteredSyms = tree.symbol :: enteredSyms
      case _ =>
    }

    /** If reference is to a locally defined symbol, check that its staging level
     *  matches the current level.
     */
    def checkLevel(tree: Tree)(implicit ctx: Context): Unit = {

      def check(sym: Symbol, show: Symbol => String): Unit =
        if (levelOf.getOrElse(sym, currentLevel) != currentLevel)
          ctx.error(em"""access to ${show(sym)} from wrong staging level:
                        | - the definition is at level ${levelOf(sym)},
                        | - but the access is at level $currentLevel.""", tree.pos)

      def showThis(sym: Symbol) = i"${sym.name}.this"

      val sym = tree.symbol
      if (sym.exists)
        if (tree.isInstanceOf[This]) check(sym, showThis)
        else if (sym.owner.isType) check(sym.owner, showThis)
        else check(sym, _.show)
    }

    /** Turn `body` of quote into a call of `scala.quoted.Unpickler.unpickleType` or
     *  `scala.quoted.Unpickler.unpickleExpr` depending onwhether `isType` is true or not.
     *  The arguments to the method are:
     *
     *    - the serialized `body`, as returned from `pickleTree`
     *    - all splices found in `body`
     */
    private def reifyCall(body: Tree, isType: Boolean)(implicit ctx: Context) =
      ref(if (isType) defn.Unpickler_unpickleType else defn.Unpickler_unpickleExpr)
        .appliedToType(if (isType) body.tpe else body.tpe.widen)
        .appliedTo(
          Literal(Constant(pickleTree(body, isType))),
          SeqLiteral(splicesAtLevel(currentLevel).toList, TypeTree(defn.QuotedType)))

    /** Perform operation `op` in quoted context */
    private def inQuote(op: => Tree)(implicit ctx: Context) = {
      currentLevel += 1
      if (currentLevel == splicesAtLevel.length) splicesAtLevel += null
      val savedSplices = splicesAtLevel(currentLevel)
      splicesAtLevel(currentLevel) = new mutable.ListBuffer[Tree]
      try op
      finally {
        splicesAtLevel(currentLevel) = savedSplices
        currentLevel -= 1
      }
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Apply(fn, arg :: Nil) if fn.symbol == defn.quoteMethod =>
        inQuote(reifyCall(transform(arg), isType = false))
      case TypeApply(fn, arg :: Nil) if fn.symbol == defn.typeQuoteMethod =>
        inQuote(reifyCall(transform(arg), isType = true))
      case Select(body, name)
      if tree.symbol == defn.QuotedExpr_~ || tree.symbol == defn.QuotedType_~ =>
        currentLevel -= 1
        val body1 = try transform(body) finally currentLevel += 1
        if (currentLevel > 0) {
          splicesAtLevel(currentLevel) += body1
          tree
        }
        else {
          if (currentLevel < 0)
            ctx.error(i"splice ~ not allowed under toplevel splice", tree.pos)
          cpy.Select(tree)(body1, name)
        }
      case (_: Ident) | (_: This) =>
        checkLevel(tree)
        super.transform(tree)
      case _: MemberDef =>
        markDef(tree)
        super.transform(tree)
      case Block(stats, _) =>
        val last = enteredSyms
        stats.foreach(markDef)
        try super.transform(tree)
        finally
          while (enteredSyms ne last) {
            levelOf -= enteredSyms.head
            enteredSyms = enteredSyms.tail
          }
      case _ =>
        super.transform(tree)
    }
  }
}