sealed trait Status
object Status {
  case object Active   extends Status
  case object Inactive extends Status
}

case class Foo(status: Status)
def bar(user: Foo): Unit = user match {
  case Foo(Status.Active) =>
    println("active")
    // no compile-time warning for missing Status.Inactive case
}
