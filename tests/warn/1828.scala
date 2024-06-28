

class Test {
  def remove[S](a: S | Int, f: Int => S):S = a match {
    case a: S => a             // warn
    case a: Int => f(a)
  }

  val t: Int | String = 5
  val t1 = remove[String](t, _.toString)
}
