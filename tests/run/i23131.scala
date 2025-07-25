import scala.NamedTuple
@main
def Test =
  Some((name = "Bob")) match {
    case Some(name = a) => println(a)
  }
//  (name = "Bob") match { // works fine
//    case (name = a) => println (a)
//  }