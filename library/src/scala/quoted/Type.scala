package scala.quoted

import scala.annotation.compileTimeOnly

/** Type (or type constructor) `T` needed contextually when using `T` in a quoted expression `'{... T ...}` */
abstract class Type[T <: AnyKind] private[scala]:
  /** The type represented `Type` */
  type Underlying = T
end Type

/** Methods to interact with the current `Type[T]` in scope */
object Type:

  /** Show a source code like representation of this type without syntax highlight */
  def show[T](using Type[T])(using Quotes): String =
    quotes.reflect.TypeTree.of[T].show

  /** Show a source code like representation of this type without syntax highlight */
  def showShort[T](using Type[T])(using Quotes): String =
    quotes.reflect.TypeTree.of[T].showShort

  /** Shows the tree as fully typed source code colored with ANSI */
  def showAnsiColored[T](using Type[T])(using Quotes): String =
    quotes.reflect.TypeTree.of[T].showAnsiColored

  /** Return a quoted.Type with the given type */
  @compileTimeOnly("Reference to `scala.quoted.Type.of` was not handled by PickleQuotes")
  given of[T <: AnyKind]: (Quotes ?=> Type[T]) = ???

end Type
