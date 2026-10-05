@main def Test =
  val errors = scala.compiletime.testing.typeCheckErrors("boom")
  errors.foreach(e => println(e.lineContent))
