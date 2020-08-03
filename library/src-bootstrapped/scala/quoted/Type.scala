package scala.quoted

import scala.annotation.compileTimeOnly
import scala.quoted.show.SyntaxHighlight

/** Staged type (or kind) `T` */
abstract class Type private[scala] {
  type T <: AnyKind

  /** Show a source code like representation of this type without syntax highlight */
  def show(using qctx: QuoteContext): String =
    this.unseal.showWith(SyntaxHighlight.plain)

  /** Show a source code like representation of this type */
  def show(syntaxHighlight: SyntaxHighlight)(using qctx: QuoteContext): String =
    this.unseal.showWith(syntaxHighlight)

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree

}

/** Staged type (or kind) `X` */
type Staged[X <: AnyKind] = Type { type T = X }

/** Some basic type tags, currently incomplete */
object Type {

  /** Return a quoted.Type with the given type */
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as (QuoteContext ?=> Staged[T]) = ???

  def UnitTag: QuoteContext ?=> Staged[Unit] =
    qctx.tasty.defn.UnitType.seal.asInstanceOf[quoted.Staged[Unit]]

  def BooleanTag: QuoteContext ?=> Staged[Boolean] =
    qctx.tasty.defn.BooleanType.seal.asInstanceOf[quoted.Staged[Boolean]]

  def ByteTag: QuoteContext ?=> Staged[Byte] =
    qctx.tasty.defn.ByteType.seal.asInstanceOf[quoted.Staged[Byte]]

  def CharTag: QuoteContext ?=> Staged[Char] =
    qctx.tasty.defn.CharType.seal.asInstanceOf[quoted.Staged[Char]]

  def ShortTag: QuoteContext ?=> Staged[Short] =
    qctx.tasty.defn.ShortType.seal.asInstanceOf[quoted.Staged[Short]]

  def IntTag: QuoteContext ?=> Staged[Int] =
    qctx.tasty.defn.IntType.seal.asInstanceOf[quoted.Staged[Int]]

  def LongTag: QuoteContext ?=> Staged[Long] =
    qctx.tasty.defn.LongType.seal.asInstanceOf[quoted.Staged[Long]]

  def FloatTag: QuoteContext ?=> Staged[Float] =
    qctx.tasty.defn.FloatType.seal.asInstanceOf[quoted.Staged[Float]]

  def DoubleTag: QuoteContext ?=> Staged[Double] =
    qctx.tasty.defn.DoubleType.seal.asInstanceOf[quoted.Staged[Double]]

}
