package scala.quoted

import scala.annotation.compileTimeOnly

/** Type (or type constructor) `T` needed contextually when using `T` in a quoted expression `'{... T ...}` */
abstract class Type[T <: AnyKind] private[scala]:
  /** The type represented by `Type` */
  type Underlying = T
end Type

/** Methods to interact with the current `Type[T]` in scope */
object Type:

  /** Show a source code like representation of this type without syntax highlight */
  def show[T <: AnyKind](using Type[T])(using Quotes): String =
    import quotes.reflect.*
    TypeTree.of[T].show

  /** Return a quoted.Type with the given type */
  @compileTimeOnly("Reference to `scala.quoted.Type.of` was not handled by PickleQuotes")
  given of[T <: AnyKind](using Quotes): Type[T] = ???


  /** Extracts the value of a singleton constant type.
   *  Returns Some of the value of the type if it is a singleton constant type.
   *  Returns None if the type is not a singleton constant type.
   *
   *  Example usage:
   *  ```scala
   *  ... match
   *    case '{ $mirrorExpr : Mirror.Sum { type MirroredLabel = label } } =>
   *      Type.valueOfConstant[label] // Option[String]
   *  }
   *  ```
   *  @syntax markdown
   */
  def valueOfConstant[T](using Type[T])(using Quotes): Option[T] =
    import quotes.reflect.*
    def valueOf(tpe: TypeRepr): Option[T] =
      tpe.dealias.widenTermRefByName match
        case ConstantType(const) => Some(const.value.asInstanceOf[T])
        case _ => None
    valueOf(TypeRepr.of[T])

end Type
