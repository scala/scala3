@main def main(): Unit =
  summon[0.0 =:= -0.0]   // error: Cannot prove that (0.0: Double) =:= (-0.0: Double).
  val d: 0.0 = -0.0      // error: Cannot prove that (0.0: Double) =:= (-0.0: Double).
  val d2: -0.0 = 0.0     // error: Cannot prove that (-0.0: Double) =:= (0.0: Double).
  summon[0.0f =:= -0.0f] // error: Cannot prove that (0.0f: Float) =:= (-0.0f: Float).
  val f: 0.0f = -0.0f    // error: Cannot prove that (0.0f: Float) =:= (-0.0f: Float).
  val f2: -0.0f = 0.0f   // error: Cannot prove that (-0.0f: Float) =:= (0.0f: Float).
