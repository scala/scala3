@main def Test(): Unit =
  val _ = M { summon[1 <:< Int] }
  val _ = M { summon[String <:< Any] }
