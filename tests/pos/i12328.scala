import scala.deriving.Mirror
import scala.compiletime._

trait Schema[T]

object Schema {
  inline def recurse[A <: Tuple]: List[Schema[Any]] =
    inline erasedValue[A] match {
      case _: (t *: ts) => summonInline[Schema[t]].asInstanceOf[Schema[Any]] :: recurse[ts]
      case EmptyTuple   => Nil
    }

  inline def derived[T]: Schema[T] =
    inline summonInline[Mirror.Of[T]] match {
      case m: Mirror.SumOf[T] =>
        val subTypes = recurse[m.MirroredElemTypes]
        new Schema[T] { }
      case m: Mirror.ProductOf[T] =>
        val fields = recurse[m.MirroredElemTypes]
        new Schema[T] { }
  }

  inline given gen[T]: Schema[T] = derived
}

@main def hello: Unit = {

  sealed trait Item
  object Item {
    case object A extends Item
    case object B extends Item
  }

  summon[Schema[Item]]
}
