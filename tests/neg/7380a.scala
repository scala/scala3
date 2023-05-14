import scala.deriving.Mirror

object Lib {

  trait HasFields[T]
  trait HasFieldsOrNone[T]

  given mirHasFields[T, ElemLabels <: NonEmptyTuple](using
    mir: Mirror.ProductOf[T] { type MirroredElemLabels = ElemLabels }
  ): HasFields[T]()

  given mirHasFieldsOrNone[T, ElemLabels <: Tuple](using
    mir: Mirror.ProductOf[T] { type MirroredElemLabels = ElemLabels }
  ): HasFieldsOrNone[T]()

}

object Test {

  Lib.mirHasFields[(Int, String), ("_1", "_2", "_3")] // error

  summon[Lib.HasFields[(Int, String)]] // ok

  case class NoFields()

  summon[Lib.HasFields[NoFields]] // error
  summon[Lib.HasFieldsOrNone[NoFields]] // ok

}
