package scala.quoted

import scala.annotation.compileTimeOnly

abstract class Type[T <: AnyKind] private[scala]:
  type Underlying = T

object Type:
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???
