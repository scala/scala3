object ExhaustivityWarnBugReportMinimal {
  //sealed is needed for the warning
  sealed trait FoundNode[T]/*presence of parameters is irrelevant*/
  // This also causes a warning:
  // sealed abstract class FoundNode[T]/*presence of parameters is irrelevant*/
  case class FoundFilter[T](/*presence of parameters is irrelevant*/) extends FoundNode[T]
  case class FoundTypeCase[T](/*presence of parameters is irrelevant*/) extends FoundNode[T]
  val f: Some[?] = ???
  f match {
    case x: Some[t] => //no warning
  }
  //With these variants, no warnings:
  //val v: (Some[Int], FoundNode[_]) = (???, ???)
  //val v: (Some[AnyRef], FoundNode[_]) = (???, ???)
  //val v: (Some[String], FoundNode[_]) = (???, ???)

  val v: (Some[?], FoundNode[?]) = (???, ???)
  //Warning here:
  v match {
    case (x: Some[t], _: FoundNode[?]) =>
  }
  v match {
    case (x: Some[t], _) =>
  }

  v match {
    case (x: Some[?], _) =>
  }
  case class Foo[T]()

  val vp: (Foo[?], FoundNode[?]) = (???, ???)
  vp match {
    case (x: Foo[?], _) =>
  }

  //No warning here:
  v match {
    case (Some(y), _) =>
  }

  v match {
    case (x, _) =>
  }

  val v2: (Some[?], Int) = (???, ???)
  v2 match {
    case (x: Some[t], _) =>
  }

  val v3: (Option[?], FoundNode[?]) = (???, ???)
  v match {
    case (x: Option[?], _) =>
  }
}
