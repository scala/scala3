package scala.quoted

abstract class Type private[scala]:
  type `$splice`
  type T
  def unseal(using qctx: QuoteContext): qctx.tasty.TypeTree

type Staged[X <: AnyKind] = Type { type T = X }
