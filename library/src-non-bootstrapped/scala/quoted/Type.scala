package scala.quoted

abstract class Type[T <: AnyKind] private[scala]:
  type `$splice` = T
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree
