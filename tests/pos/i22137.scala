enum Parser[+Value]:
  case Success(value: Value, issues: Seq[Failure] = Seq.empty) extends Parser[Value]
  case Failure(exception: Throwable)                           extends Parser[Nothing]

val v = Parser.Success(1)