import language.strictEquality
object equality1 {
  class A
  class B
  new A == new B // error: cannot compare

  case class Foo(n: Int) derives CanEqual

  sealed trait Status derives CanEqual
  object Status {
    case class Active(since: Int) extends Status
    case object Pending extends Status
    case object Inactive extends Status
  }

  enum Color derives CanEqual {
    case Red
    case Green
    case Blue
  }

  val option1a: Option[Int] = Some(1)
  val option1b: Option[Int] = Some(1)
  option1a == option1b

  option1a match {
    case Some(1) =>
      println("1")
    case Some(n) =>
      println("Not 1")
    case None => // This None case doesn't work without CanEqual.canEqualOption[T]
      println("None")
  }

  1 == '1'
  val option2a: Option[Int] = Some(1)
  val option2b: Option[Char] = Some('1')
  option2a == option2b

  val option3a: Option[Foo] = Some(Foo(1))
  val option3b: Option[Foo] = Some(Foo(1))
  option3a == option3b

  val option4a: Option[Status] = Some(Status.Active(2020))
  val option4b: Option[Status] = Some(Status.Pending)
  val option4c: Option[Status] = Some(Status.Inactive)
  option4a == option4b
  option4b == option4c

  val option5a: Option[Color] = Some(Color.Red)
  val option5b: Option[Color] = Some(Color.Green)
  val option5c: Option[Color] = Some(Color.Blue)
  option5a == option5b
  option5b == option5c

  val optionError1a: Option[Int] = Some(1)
  val optionError1b: Option[String] = Some("1")
  optionError1a == optionError1b // error: cannot compare

  val optionError2a: Option[Char] = Some('a')
  val optionError2b: Option[String] = Some("a")
  optionError2a == optionError2b // error: cannot compare

  val optionTuple1a: Option[(Int, String)] = Some((1, "OK"))
  val optionTuple1b: Option[(Int, String)] = Some((1, "OK"))
  optionTuple1a == optionTuple1b

  'a' == 97
  val optionTuple2a: Option[(Int, Char)] = Some((1, 'a'))
  val optionTuple2b: Option[(Int, Int)] = Some((1, 97))
  optionTuple2a == optionTuple2b

  val optionTupleError1a: Option[(Int, String)] = Some((1, "OK"))
  val optionTupleError1b: Option[(String, Int)] = Some(("OK", 1))
  optionTupleError1a == optionTupleError1b // error: cannot compare

  val eitherL1a: Either[String, Int] = Left("Error")
  val eitherL1b: Either[String, Int] = Left("Error")
  eitherL1a == eitherL1b

  val eitherR1a: Either[String, Int] = Right(999)
  val eitherR1b: Either[String, Int] = Right(999)
  eitherR1a == eitherR1b

  val eitherErrorL1a: Either[String, Int] = Left("Error")
  val eitherErrorL1b: Either[Char, Int] = Left('E')
  eitherErrorL1a == eitherErrorL1b // error: cannot compare

  val eitherErrorR1a: Either[String, Int] = Right(999)
  val eitherErrorR1b: Either[String, String] = Right("999")
  eitherErrorR1a == eitherErrorR1b // error: cannot compare


  val eitherTupleL1a: Either[(String, Long), (Int, Boolean)] = Left(("Error", 123L))
  val eitherTupleL1b: Either[(String, Long), (Int, Boolean)] = Left(("Error", 123L))
  eitherTupleL1a == eitherTupleL1b

  val eitherTupleR1a: Either[(String, Long), (Int, Boolean)] = Right((999, true))
  val eitherTupleR1b: Either[(String, Long), (Int, Boolean)] = Right((999, true))
  eitherTupleR1a == eitherTupleR1b

  val eitherTupleErrorL1a: Either[(String, Long), (Int, Boolean)] = Left(("Error", 123L))
  val eitherTupleErrorL1b: Either[(Long, String), (Int, Boolean)] = Left((123L, "Error"))
  eitherTupleErrorL1a == eitherTupleErrorL1b // error: cannot compare

  val eitherTupleErrorR1a: Either[(String, Long), (Int, Boolean)] = Right((999, true))
  val eitherTupleErrorR1b: Either[(String, Long), (Boolean, Int)] = Right((true, 999))
  eitherTupleErrorR1a == eitherTupleErrorR1b // error: cannot compare

  (1, "a") == (1, "a")
  (1, "a", true) == (1, "a", true)
  (1, "a", true, 't') == (1, "a", true, 't')
  (1, "a", true, 't', 10L) == (1, "a", true, 't', 10L)

  (1, "a") == (1, 'a') // error: cannot compare
  (1, "a") == ("a", 1) // error: cannot compare
  (1, "a") == (1, "a", true)  // error: cannot compare
  (1, "a", true, 't', 10L) == (1, "a", 1.5D, 't', 10L) // error: cannot compare

}
