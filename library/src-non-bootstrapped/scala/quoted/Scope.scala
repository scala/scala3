package scala.quoted

trait Scope { self =>

  type Expr[+T]
  type Type[T <: AnyKind]

  val tasty: scala.tasty.Reflection

  type Nested = Scope {
    val tasty: self.tasty.type
  }

}
