//> using options -source:3.5

import scala.deriving.*
import scala.quoted.*

trait SName
abstract class CaseClass[Typeclass[_], Type]:
  def param: CaseClass.Param[Typeclass, Type]

object CaseClass:
  trait Param[Typeclass[_], Type]:
    type PType
    def typeclass: Typeclass[PType]


sealed trait IsUnionOf[T, A]
object IsUnionOf:
  transparent inline given derived[T, A]: IsUnionOf[T, A] = ${ deriveImpl[T, A] }
  private def deriveImpl[T, A](using quotes: Quotes): Expr[IsUnionOf[T, A]] = ???

trait SchemaDerivation:
  inline implicit def derived[T](implicit m: Mirror.Of[T]): Schema[T] =
    val ctx: CaseClass[Schema, T] = ???
    val valueSchema = ctx.param.typeclass
    val format = valueSchema.format
    ???
