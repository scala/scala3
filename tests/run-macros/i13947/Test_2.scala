@main def Test: Unit =
  printTypeParams[scala.util.Random]
  printTypeParams[Option[String]]
  printTypeParams[Function2[Int, Float, Long]]
