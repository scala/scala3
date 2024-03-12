class Outer:
  case class Inner(a: String, b: Int = 23) derives Defaults

object Test:
  def main(args: Array[String]): Unit =
    val outer = Outer()
    println(summon[Defaults[outer.Inner]].defaults)
