package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{Type, ExprType}
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils._

import scala.collection.JavaConverters._
import scala.collection.mutable

import java.util.IdentityHashMap

object CollectNullableFields {
  val name = "collectNullableFields"
}

/** Collect fields that can be null out after use in lazy initialization.
 *
 *  This information is used during lazy val transformation to assign null to private
 *  fields that are only used within a lazy val initializer. This is not just an optimization,
 *  but is needed for correctness to prevent memory leaks. E.g.
 *
 *  {{{
 *  class TestByNameLazy(byNameMsg: => String) {
 *    lazy val byLazyValMsg = byNameMsg
 *  }
 *  }}}
 *
 *  Here `byNameMsg` should be null out once `byLazyValMsg` is
 *  initialised.
 *
 *  A field is nullable if all the conditions below hold:
 *    - belongs to a non trait-class
 *    - is private
 *    - is not lazy
 *    - its type is nullable, or is an expression type (e.g. => Int)
 *    - is on used in a lazy val initializer
 *    - defined in the same class as the lazy val
 */
class CollectNullableFields extends MiniPhase {
  import tpd._

  override def phaseName = CollectNullableFields.name

  private[this] sealed trait FieldInfo
  private[this] case object NotNullable extends FieldInfo
  private[this] case class Nullable(by: Symbol) extends FieldInfo

  /** Whether or not a field is nullable */
  private[this] var nullability: IdentityHashMap[Symbol, FieldInfo] = _

  override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
    nullability = new IdentityHashMap
    ctx
  }

  private def recordUse(tree: Tree)(implicit ctx: Context): Tree = {
    val sym = tree.symbol

    def isNullableType(tpe: Type) =
      tpe.isInstanceOf[ExprType] ||
      tpe.widenDealias.typeSymbol.isNullableClass
    val isNullablePrivateField =
      sym.isField &&
      sym.is(Private, butNot = Lazy) &&
      !sym.owner.is(Trait) &&
      isNullableType(sym.info)

    if (isNullablePrivateField)
      nullability.get(sym) match {
        case Nullable(from) if from != ctx.owner => // used in multiple lazy val initializers
          nullability.put(sym, NotNullable)
        case null => // not in the map
          val from = ctx.owner
          val isNullable =
            from.is(Lazy) && from.isField && // used in lazy field initializer
            from.owner.eq(sym.owner)         // lazy val and field in the same class
          val info = if (isNullable) Nullable(from) else NotNullable
          nullability.put(sym, info)
        case _ =>
          // Do nothing for:
          //  - NotNullable
          //  - Nullable(ctx.owner)
      }

    tree
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context) =
    recordUse(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context) =
    recordUse(tree)

  /** Map lazy values to the fields they should null after initialization. */
  def lazyValNullables(implicit ctx: Context): Map[Symbol, List[Symbol]] = {
    val result = new mutable.HashMap[Symbol, mutable.ListBuffer[Symbol]]

    nullability.forEach {
      case (sym, Nullable(from)) =>
        val bldr = result.getOrElseUpdate(from, new mutable.ListBuffer)
        bldr += sym
      case _ =>
    }

    result.mapValues(_.toList).toMap
  }
}
