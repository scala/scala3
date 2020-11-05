package scala.quoted

import scala.annotation.compileTimeOnly

/** Quoted type (or kind) `T` */
abstract class Type[T <: AnyKind] private[scala] {

  /** The type represented `Type` */
  type Underlying = T

  /** Show a source code like representation of this type without syntax highlight */
  def show(using qctx: QuoteContext): String = this.unseal.show

  /** Shows the tree as fully typed source code colored with ANSI */
  def showAnsiColored(using qctx: QuoteContext): String = this.unseal.showAnsiColored

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def unseal(using qctx: QuoteContext): qctx.reflect.TypeTree

}

/** Some basic type tags, currently incomplete */
object Type {

  /** Return a quoted.Type with the given type */
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???

}
