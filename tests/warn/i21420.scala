//> using options -Wunused:imports

object decisions4s{
  trait HKD
  trait DecisionTable
}

object DiagnosticsExample {
  import decisions4s.HKD
  val _ = new HKD {}
  import decisions4s.*
  val _ = new DecisionTable {}
}
