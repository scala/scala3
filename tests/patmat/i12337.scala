sealed trait Status
object Status {
  case class Active(since: Int) extends Status
  case object Inactive extends Status
}

case class Foo(status: Status)
def bar(foo: Foo): Unit = foo match {
  case Foo(Status.Active(since)) =>
    println(s"active since $since")
}
// Expected:
// warning: match may not be exhaustive.
// It would fail on the following input: Foo(Inactive)
// def bar(foo: Foo): Unit = foo match {

def baz(foo: Foo): Unit = foo match {
  case Foo(Status.Active(2000)) =>
    println("active since 2000")
  case Foo(Status.Inactive) =>
    println("inactive")
}
// Expected:
// warning: match may not be exhaustive.
// It would fail on the following input: Foo(Active((x: Int forSome x not in 2000)))
// def baz(foo: Foo): Unit = foo match {