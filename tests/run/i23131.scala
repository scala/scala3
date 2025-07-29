import scala.NamedTuple
@main
def Test =
  Some((name = "Bob")) match {
    case Some(name = a) => println(a)
  }