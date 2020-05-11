package scala.quoted

class Type[T <: AnyKind] private[scala]:
  type `$splice` = T
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree =
    throw new Exception("Non bootstrapped library")
