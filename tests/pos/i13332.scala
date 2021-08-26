import scala.deriving.Mirror

trait IListDefn {
  sealed trait IList[A]
  case class INil[A]() extends IList[A]
  case class ICons[A](h: A, t: IList[A]) extends IList[A]
}

class ZListDefn {
  sealed trait ZList
  case class ZNil() extends ZList
  case class ZCons(h: Boolean, t: ZList) extends ZList
}

class Scope extends IListDefn {

  type Of = Mirror { type MirroredType[X] = IList[X]; type MirroredMonoType = IList[Any] ; type MirroredElemTypes[_] <: Tuple }

  val M = summon[Of]
}

class Scope2 extends ZListDefn {

  type Of = Mirror { type MirroredType = ZList; type MirroredMonoType = ZList ; type MirroredElemTypes <: Tuple }

  val N = summon[Of]

}
