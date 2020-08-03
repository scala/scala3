package scala.quoted

abstract class QuotedType private[scala]:
  type `$splice`
  type T <: AnyKind
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree

type Type[X <: AnyKind] = QuotedType { type T = X }
