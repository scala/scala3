val l: List[Option[Int]] = List(None, Some(1), None)

@main def m15 =
  l.collectFirst(Some.unapply.unlift[Option[Int], Int]) // error
