package scala.quoted

abstract class Type[T <: AnyKind] private[scala]:
  type `$splice` = T
  def asTypeTree(using qctx: QuoteContext): qctx.tasty.TypeTree
