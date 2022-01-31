package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.SymUtils._

import scala.collection.mutable

import java.util.IdentityHashMap

object CollectNullableFields {
  val name: String = "collectNullableFields"
  val description: String = "collect fields that can be nulled out after use in lazy initialization"
}

/** Collect fields that can be nulled out after use in lazy initialization.
 *
 *  This information is used during lazy val transformation to assign null to private
 *  fields that are only used within a lazy val initializer. This is not just an optimization,
 *  but is needed for correctness to prevent memory leaks. E.g.
 *
 *  ```scala
 *  class TestByNameLazy(byNameMsg: => String) {
 *    lazy val byLazyValMsg = byNameMsg
 *  }
 *  ```
 *
 *  Here `byNameMsg` should be null out once `byLazyValMsg` is
 *  initialised.
 *
 *  A field is nullable if all the conditions below hold:
 *    - belongs to a non trait-class
 *    - is private[this]
 *    - is not lazy
 *    - its type is nullable after erasure
 *    - is only used in a lazy val initializer
 *    - defined in the same class as the lazy val
 */
class CollectNullableFields extends MiniPhase {
  import tpd._

  override def phaseName: String = CollectNullableFields.name

  override def description: String = CollectNullableFields.description

  /** Running after `ElimByName` to see by names as nullable types. */
  override def runsAfter: Set[String] = Set(ElimByName.name)

  private sealed trait FieldInfo
  private case object NotNullable extends FieldInfo
  private case class Nullable(by: Symbol) extends FieldInfo

  /** Whether or not a field is nullable */
  private val nullability = new mutable.LinkedHashMap[Symbol, FieldInfo]

  private def recordUse(tree: Tree)(using Context): Tree = {
    val sym = tree.symbol
    val isNullablePrivateField =
      sym.isField &&
      !sym.is(Lazy) &&
      !sym.owner.is(Trait) &&
      sym.initial.isAllOf(PrivateLocal) &&
      // We need `isNullableClassAfterErasure` and not `isNullable` because
      // we care about the values as present in the JVM.
      sym.info.widenDealias.typeSymbol.isNullableClassAfterErasure

    if (isNullablePrivateField)
      nullability.get(sym) match {
        case Some(Nullable(from)) if from != ctx.owner => // used in multiple lazy val initializers
          nullability.put(sym, NotNullable)
        case None => // not in the map
          val from = ctx.owner
          val isNullable =
            from.is(Lazy, butNot = Module) && // is lazy val
            from.owner.isClass &&             // is field
            from.owner.eq(sym.owner)          // is lazy val and field defined in the same class
          val info = if (isNullable) Nullable(from) else NotNullable
          nullability.put(sym, info)
        case _ =>
          // Do nothing for:
          //  - NotNullable
          //  - Nullable(ctx.owner)
      }

    tree
  }

  override def transformIdent(tree: Ident)(using Context): Tree =
    recordUse(tree)

  override def transformSelect(tree: Select)(using Context): Tree =
    recordUse(tree)

  /** Map lazy values to the fields they should null after initialization. */
  def lazyValNullables(using Context): IdentityHashMap[Symbol, mutable.ListBuffer[Symbol]] = {
    val result = new IdentityHashMap[Symbol, mutable.ListBuffer[Symbol]]

    nullability.foreach {
      case (sym, Nullable(from)) =>
        val bldr = result.computeIfAbsent(from, _ => new mutable.ListBuffer)
        bldr += sym
      case _ =>
    }

    result
  }
}
