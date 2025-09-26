trait Nodes1 {
  sealed trait B
  final case class R1() extends B
}

trait Nodes2 extends Nodes1 {
  final case class R2[T]() extends B
}


object Impl1 extends Nodes1

object test2 {
  val a: Impl1.B = ???
  a match {
    case Impl1.R1() => ???
  }
}
