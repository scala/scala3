trait Universe {
  type Name >: Null <: AnyRef & NameApi
  trait NameApi

  type TermName >: Null <: TermNameApi & Name
  trait TermNameApi extends NameApi
}

object Test extends App {
  val u: Universe = ???
  import u.*

  def foo(name: Name) = ???
  def foo(name: TermName) = ??? // error: double definition, same type after erasure
}
