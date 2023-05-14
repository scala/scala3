package scala.quoted

import scala.annotation.{compileTimeOnly, experimental}

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
   *  //{
   *  import scala.deriving.*
   *  def f(using Quotes) = {
   *    import quotes.reflect.*
   *    val expr: Expr[Any] = ???
   *  //}
   *    expr match {
   *      case '{ $mirrorExpr : Mirror.Sum { type MirroredLabel = label } } =>
   *        Type.valueOfConstant[label] // Option[String]
   *    }
   *  //{
   *  }
   *  //}
   *  ```
   */
  def valueOfConstant[T](using Type[T])(using Quotes): Option[T] =
    ValueOf.unapply(quotes.reflect.TypeRepr.of[T]).asInstanceOf[Option[T]]

  /** Extracts the value of a tuple of singleton constant types.
   *  Returns Some of the tuple type if it is a tuple singleton constant types.
   *  Returns None if the type is not a tuple singleton constant types.
   *
   *  Example usage:
   *  ```scala
   *  //{
   *  import scala.deriving.*
   *  def f(using Quotes) = {
   *    import quotes.reflect.*
   *    val expr: Expr[Any] = ???
   *  //}
   *    expr match {
   *      case '{ type label <: Tuple; $mirrorExpr : Mirror.Sum { type MirroredElemLabels = `label` } } =>
   *        Type.valueOfTuple[label] // Option[Tuple]
   *    }
   *  //{
   *  }
   *  //}
   *  ```
   */
  def valueOfTuple[T <: Tuple](using Type[T])(using Quotes): Option[T] =
    valueOfTuple(quotes.reflect.TypeRepr.of[T]).asInstanceOf[Option[T]]

  private def valueOfTuple(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Tuple] =
    import quotes.reflect.*
    val cons = Symbol.classSymbol("scala.*:")
    def rec(tpe: TypeRepr): Option[Tuple] =
      tpe.widenTermRefByName.dealias match
        case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
          tpes.foldRight(Option[Tuple](EmptyTuple)) {
            case (_, None) => None
            case (ValueOf(v), Some(acc)) => Some(v *: acc)
            case _ => None
          }
        case AppliedType(tp, List(ValueOf(headValue), tail)) if tp.derivesFrom(cons) =>
          rec(tail) match
            case Some(tailValue) => Some(headValue *: tailValue)
            case None => None
        case tpe =>
          if tpe.derivesFrom(Symbol.classSymbol("scala.EmptyTuple")) then Some(EmptyTuple)
          else None
    rec(tpe)

  private object ValueOf:
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Any] =
      import quotes.reflect.*
      tpe.widenTermRefByName.dealias match
        case ConstantType(const) => Some(const.value)
        case _ => None

end Type
