// cf. pos/t8300-patmat-b.scala
trait Universe {
  type Name >: Null <: AnyRef & NameApi
  trait NameApi

  type TermName >: Null <: Name & TermNameApi
  trait TermNameApi extends NameApi
}

object Test extends App {
  val u: Universe = ???
  import u.*

  locally {
    val ScalaName: TermName = ???
    ??? match {
      case ScalaName => ???
    }
  }
}
