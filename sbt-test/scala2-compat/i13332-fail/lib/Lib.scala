package collectionstrawman

class ListModule {

  sealed trait IList[+A]
  case class ICons[+A](head: A, next: IList[A]) extends IList[A]
  case object INil extends IList[Nothing]

  case class IPair[+A, +B](a: A, b: B)

  case object IUnit

}
