package scala.quoted

import scala.annotation.compileTimeOnly

abstract class Type[X <: AnyKind] private[scala]:
  type T = X
  def unseal(using s: Scope): s.tasty.TypeTree

object Type:
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as ((s: Scope) ?=> s.Type[T]) = ???
