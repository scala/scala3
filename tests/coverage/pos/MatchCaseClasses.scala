package covtest

case class Pat1(x: Int)
case class Pat2(x: Int, y: Any)

object MatchCaseClasses:
  def f(x: Any): Unit = x match
    case Pat1(0) => println("a")
    case Pat1(_) => println("b")
    case p @ Pat2(1, -1) => println("c")
    case Pat2(_, y: String) =>
      println(y)
      println("d")
    case p: Pat2 => println("e")
    case _ => println("other")
