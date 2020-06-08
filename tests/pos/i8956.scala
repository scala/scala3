type Numeric = Double | Int

val v1 = 100
val v2 = 100.0
def check1(i: Double | Int | String): Unit = {
  i match {
    case a:(Double | Int) => println(s"numeric = $a")
    case _ => println("categorical")
  }
}
/*
  [warn] 25 |      case a:Numeric => println(s"numeric = $a")
  [warn]    |           ^^^^^^^^^
  [warn]    |  the type test for gg.SlidingIssue.Numeric cannot be checked at runtime
  [warn] one warning found
  */
def check2(i: Double | Int | String): Unit = {
  i match {
    case a:Numeric => println(s"numeric = $a")
    case _ => println("categorical")
  }
}
