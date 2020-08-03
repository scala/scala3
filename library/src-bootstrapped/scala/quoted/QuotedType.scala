package scala.quoted

import scala.annotation.compileTimeOnly
import scala.quoted.show.SyntaxHighlight

/** Quoted type (or kind) `X` */
type Type[X <: AnyKind] = QuotedType { type T = X }

/** A TypeTree with a known type (or kind) `T` */
abstract class QuotedType private[scala] {
  type T <: AnyKind

  /** Show a source code like representation of this type without syntax highlight */
  def show(using qctx: QuoteContext): String =
    this.unseal.showWith(SyntaxHighlight.plain)

  /** Show a source code like representation of this type */
  def showWith(syntaxHighlight: SyntaxHighlight)(using qctx: QuoteContext): String =
    this.unseal.showWith(syntaxHighlight)

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree

}

/** Some basic type tags, currently incomplete */
object QuotedType {

  /** Return a quoted.Type with the given type */
  @compileTimeOnly("Reference to `scala.quoted.QuotedType.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???


  // TODO: Move Tags
  //       these are here for a compiler optimization not for the user. users should write '[Int] directly
  //       they should be in sclaa.internal.quoted.Type

  def UnitTag: QuoteContext ?=> Type[Unit] =
    qctx.tasty.defn.UnitType.seal.asInstanceOf[quoted.Type[Unit]]

  def BooleanTag: QuoteContext ?=> Type[Boolean] =
    qctx.tasty.defn.BooleanType.seal.asInstanceOf[quoted.Type[Boolean]]

  def ByteTag: QuoteContext ?=> Type[Byte] =
    qctx.tasty.defn.ByteType.seal.asInstanceOf[quoted.Type[Byte]]

  def CharTag: QuoteContext ?=> Type[Char] =
    qctx.tasty.defn.CharType.seal.asInstanceOf[quoted.Type[Char]]

  def ShortTag: QuoteContext ?=> Type[Short] =
    qctx.tasty.defn.ShortType.seal.asInstanceOf[quoted.Type[Short]]

  def IntTag: QuoteContext ?=> Type[Int] =
    qctx.tasty.defn.IntType.seal.asInstanceOf[quoted.Type[Int]]

  def LongTag: QuoteContext ?=> Type[Long] =
    qctx.tasty.defn.LongType.seal.asInstanceOf[quoted.Type[Long]]

  def FloatTag: QuoteContext ?=> Type[Float] =
    qctx.tasty.defn.FloatType.seal.asInstanceOf[quoted.Type[Float]]

  def DoubleTag: QuoteContext ?=> Type[Double] =
    qctx.tasty.defn.DoubleType.seal.asInstanceOf[quoted.Type[Double]]

}
