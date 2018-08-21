object Test {

  def lookup(): Option[Tuple2[String, String]] =
    ((??? : Option[Tuple2[String, String]]) : @unchecked) match {
      case Some((_, _)) =>
    if (true)
      Some((???, ???))
    else
      lookup() match {
        case Some(_) => Some(???)
        case None => None
      }
    }
}
