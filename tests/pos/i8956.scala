//> using options -Werror -deprecation -feature

type Numeric = Double | Int

val v1 = 100
val v2 = 100.0
def check1(i: Double | Int | String): Unit = {
  i match {
    case a:(Double | Int) => println(s"numeric = $a")
    case _ => println("categorical")
  }
}
def check2(i: Double | Int | String): Unit = {
  i match {
    case a:Numeric => println(s"numeric = $a")
    case _ => println("categorical")
  }
}
