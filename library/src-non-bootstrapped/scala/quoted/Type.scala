package scala.quoted

import scala.annotation.compileTimeOnly

abstract class Type[T <: AnyKind] private[scala]:
  type Underlying = T

object Type:
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by PickleQuotes")
  given of[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???
  given apply[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???
