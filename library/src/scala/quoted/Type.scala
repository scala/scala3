package scala.quoted

import scala.quoted.show.SyntaxHighlight

/** Quoted type (or kind) `T` */
class Type[T <: AnyKind] private[scala] {
  type `$splice` = T

  /** Show a source code like representation of this type without syntax highlight */
  def show(using qctx: QuoteContext): String =
    this.unseal.showWith(SyntaxHighlight.plain)

  /** Show a source code like representation of this type */
  def show(syntaxHighlight: SyntaxHighlight)(using qctx: QuoteContext): String =
    this.unseal.showWith(syntaxHighlight)

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree =
    qctx.tasty.internal.QuotedType_unseal(this)(using qctx.tasty.rootContext)

}

/** Some basic type tags, currently incomplete */
object Type {

  given UnitTag(using qctx: QuoteContext) as Type[Unit] =
    qctx.tasty.defn.UnitType.seal.asInstanceOf[quoted.Type[Unit]]

  given BooleanTag(using qctx: QuoteContext) as Type[Boolean] =
    qctx.tasty.defn.BooleanType.seal.asInstanceOf[quoted.Type[Boolean]]

  given ByteTag(using qctx: QuoteContext) as Type[Byte] =
    qctx.tasty.defn.ByteType.seal.asInstanceOf[quoted.Type[Byte]]

  given CharTag(using qctx: QuoteContext) as Type[Char] =
    qctx.tasty.defn.CharType.seal.asInstanceOf[quoted.Type[Char]]

  given ShortTag(using qctx: QuoteContext) as Type[Short] =
    qctx.tasty.defn.ShortType.seal.asInstanceOf[quoted.Type[Short]]

  given IntTag(using qctx: QuoteContext) as Type[Int] =
    qctx.tasty.defn.IntType.seal.asInstanceOf[quoted.Type[Int]]

  given LongTag(using qctx: QuoteContext) as Type[Long] =
    qctx.tasty.defn.LongType.seal.asInstanceOf[quoted.Type[Long]]

  given FloatTag(using qctx: QuoteContext) as Type[Float] =
    qctx.tasty.defn.FloatType.seal.asInstanceOf[quoted.Type[Float]]

  given DoubleTag(using qctx: QuoteContext) as Type[Double] =
    qctx.tasty.defn.DoubleType.seal.asInstanceOf[quoted.Type[Double]]

}
