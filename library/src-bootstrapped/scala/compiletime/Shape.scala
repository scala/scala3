package scala.compiletime

/** The shape of an ADT.
 *  This is either a product (`Case`) or a sum (`Cases`) of products.
 */
sealed abstract class Shape

object Shape {

  /** A sum with alternative types `Alts` */
  case class Cases[Alts <: Tuple] extends Shape

  /** A product type `T` with element types `Elems` */
  case class Case[T, Elems <: Tuple] extends Shape
}

