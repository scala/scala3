import scala.deriving.Mirror

class Scope extends IListDefn {

  type Of = Mirror { type MirroredType[X] = IList[X]; type MirroredMonoType = IList[Any] ; type MirroredElemTypes[_] <: Tuple }

  val M = summon[Of]
}


trait IListDefn {
  sealed trait IList[A]
  case class INil[A]() extends IList[A]
  case class ICons[A](h: A, t: IList[A]) extends IList[A]
}
