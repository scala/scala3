//> using options -Werror -Wunused:imports

object decisions4s{
  trait HKD
  trait DecisionTable
}

object DiagnosticsExample {
  import decisions4s.HKD
  case class Input[F[_]]() extends HKD
  import decisions4s.*
  val decisionTable: DecisionTable = ???
}
