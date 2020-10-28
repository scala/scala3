package scala.quoted

import scala.annotation.compileTimeOnly

abstract class Type[T <: AnyKind] private[scala]:
  type Underlying = T

  def unseal(using qctx: QuoteContext): qctx.reflect.TypeTree

object Type:
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???
