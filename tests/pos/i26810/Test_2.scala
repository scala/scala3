@main def Test(): Unit =
  val _ = Tests {
    "x" - {
      summon[1 <:< Int]
    }
  }
