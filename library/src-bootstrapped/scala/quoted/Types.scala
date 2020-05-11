package scala.quoted

import scala.quoted.show.SyntaxHighlight

trait Types { self: Scope =>

  /** Type tree annottated with its know type (or kind) `X` */
  type Type[T <: AnyKind] <: tasty.TypeTree {
    // TODO docs
    type X = T
  }

  object Type:
    // TODO docs
    given apply[T <: AnyKind] as Type[T] = ???

}
