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
