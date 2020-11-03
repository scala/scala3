package scala.quoted

import scala.annotation.compileTimeOnly
import scala.quoted.reflect.printers.SyntaxHighlight

/** Quoted type (or kind) `T` */
abstract class Type[T <: AnyKind] private[scala] {

  /** The type represented `Type` */
  type Underlying = T

  /** Show a source code like representation of this type without syntax highlight */
  def show(using qctx: QuoteContext): String =
    this.unseal.showWith(SyntaxHighlight.plain)

  /** Show a source code like representation of this type */
  def showWith(syntaxHighlight: SyntaxHighlight)(using qctx: QuoteContext): String =
    this.unseal.showWith(syntaxHighlight)

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def unseal(using qctx: QuoteContext): qctx.reflect.TypeTree

}

/** Some basic type tags, currently incomplete */
object Type {

  /** Return a quoted.Type with the given type */
  @compileTimeOnly("Reference to `scala.quoted.Type.apply` was not handled by ReifyQuotes")
  given apply[T <: AnyKind] as (QuoteContext ?=> Type[T]) = ???

}
