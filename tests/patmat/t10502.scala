object Perhaps {

  def unapply[A](oa: Option[A]): Some[Option[A]] = Some(oa)

  Option("hello") match {
    case Perhaps(Some(s)) => println(s)
  }

  List(Option("hello")) match {
    case Perhaps(Some(s)) :: t => println(s)
    case Perhaps(None   ) :: t => ()
    case Nil                   => ()
  }

  List(Option("hello")) match {
    case Perhaps(Some(s)) :: t => println(s)
    case Perhaps(None   ) :: t => ()
    // case Nil                   => ()
  }

}

object Multi {
  def unapply(str: String): Some[(Option[Int], Int)] = ???

  "hello" match {
     case Multi(Some(i), x) =>
     case Multi(None, x) =>
  }

  "hello" match {
     case Multi(Some(i), x) =>
  }
}
