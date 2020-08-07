package scala.quoted

import scala.annotation.compileTimeOnly
import scala.quoted.show.SyntaxHighlight

/** Quoted type (or kind) `T` */
abstract class Type[X <: AnyKind] private[scala] {
  type T = X

  /** Show a source code like representation of this type without syntax highlight */
  def show(using qctx: QuoteContext): String =
    this.asTypeTree.showWith(SyntaxHighlight.plain)

  /** Show a source code like representation of this type */
  def showWith(syntaxHighlight: SyntaxHighlight)(using qctx: QuoteContext): String =
    this.asTypeTree.showWith(syntaxHighlight)

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree = asTypeTree

  /** View this expression `quoted.Type[T]` as a `qctx.tasty.TypeTree` */
  def asTypeTree(using qctx: QuoteContext): qctx.tasty.TypeTree

}

/** Some basic type tags, currently incomplete */
object Type {

  /** Return a quoted.Type with the given type */
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???

  def UnitTag: QuoteContext ?=> Type[Unit] =
    qctx.tasty.defn.UnitType.asQuotedType.asInstanceOf[quoted.Type[Unit]]

  def BooleanTag: QuoteContext ?=> Type[Boolean] =
    qctx.tasty.defn.BooleanType.asQuotedType.asInstanceOf[quoted.Type[Boolean]]

  def ByteTag: QuoteContext ?=> Type[Byte] =
    qctx.tasty.defn.ByteType.asQuotedType.asInstanceOf[quoted.Type[Byte]]

  def CharTag: QuoteContext ?=> Type[Char] =
    qctx.tasty.defn.CharType.asQuotedType.asInstanceOf[quoted.Type[Char]]

  def ShortTag: QuoteContext ?=> Type[Short] =
    qctx.tasty.defn.ShortType.asQuotedType.asInstanceOf[quoted.Type[Short]]

  def IntTag: QuoteContext ?=> Type[Int] =
    qctx.tasty.defn.IntType.asQuotedType.asInstanceOf[quoted.Type[Int]]

  def LongTag: QuoteContext ?=> Type[Long] =
    qctx.tasty.defn.LongType.asQuotedType.asInstanceOf[quoted.Type[Long]]

  def FloatTag: QuoteContext ?=> Type[Float] =
    qctx.tasty.defn.FloatType.asQuotedType.asInstanceOf[quoted.Type[Float]]

  def DoubleTag: QuoteContext ?=> Type[Double] =
    qctx.tasty.defn.DoubleType.asQuotedType.asInstanceOf[quoted.Type[Double]]

}
