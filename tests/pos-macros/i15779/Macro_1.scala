import scala.quoted._
import scala.deriving.Mirror

trait Encoder[-A]

trait PrimitiveEncoder[A] extends Encoder[A]

given intOpt: PrimitiveEncoder[Option[Int]] with {}

given primitiveNotNull[T](using e: Encoder[Option[T]]): PrimitiveEncoder[T] =
  new PrimitiveEncoder[T] {}

transparent inline given fromMirror[A]: Any = ${ fromMirrorImpl[A] }

def fromMirrorImpl[A : Type](using q: Quotes): Expr[Any] =
  Expr.summon[Mirror.Of[A]].get match
    case '{ ${mirror}: Mirror.ProductOf[A] { type MirroredElemTypes = elementTypes } } =>
      val encoder = Type.of[elementTypes] match
        case '[tpe *: EmptyTuple] =>
          Expr.summon[Encoder[tpe]].get

      encoder match
        case '{ ${encoder}: Encoder[tpe] } => // ok
        case _ => ???

      encoder match
        case '{ ${encoder}: Encoder[tpe] } => // ok
        case _ => ???

      encoder
