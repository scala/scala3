import scala.annotation.showAsInfix

object Test {
  trait Component
  sealed trait Deleted extends Component

  type Deletable[L <: CList] <: CList = L match {
    case h &: t => (h | Deleted) &: Deletable[t]
    case CNil => CNil
  }

  sealed trait CList
  sealed trait CNil extends CList
  @showAsInfix case class &:[+C <: Component, +L <: CList](h: C, t: L) extends CList

  case class A(x: Int, y: Int) extends Component
  case class B(x: Int, y: Int) extends Component

  val x: Deletable[A &: B &: CNil] = ???
}
