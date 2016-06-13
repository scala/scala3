
object Test {
  def main(args: Array[String]) = {

    1 match {
      case 1 => println("OK")
    }

    1 match {
      case 1 if false => println("fail")
      case _          => println("OK")
    }

    "a" match {
      case "a" => println("OK")
    }

    "a" match {
      case "a" if false => println("fail")
      case _            => println("OK")
    }

    def match1(e: Int) = e match {
      case 0 => println("0")
      case 1 => println("1")
      case 2 => println("2")
      case _ => println("3")
    }

    println("match1")
    (0 to 3).foreach(println)

    def match2(e: Int, guard: Boolean = true) = e match {
      case 0          => println("0")
      case 1          => println("1")
      case 2 if guard => println("2")
      case 2          => println("3")
      case 3          => println("4")
      case 4          => println("5")
      case 5 if guard => println("6")
      case _ if guard => println("7")
      case _          => println("8")
    }

    println("match2")
    match2(0)
    match2(1)
    match2(2, true)
    match2(2, false)
    match2(3)
    match2(4)
    match2(5, true)
    match2(5, false)
    match2(6, true)
    match2(6, false)

    def match3(e: List[Int]) = e match {
      case List(1)      => println("0")
      case List(1, 2)   => println("1")
      case head :: tail => println("2")
      case Nil          => println("3")
    }

    println("match3")
    match3(List(1))
    match3(List(1, 2))
    match3(List(1, 2, 3))
    match3(List())

    def match4(e: Any, guard: Boolean = true) =
      try {
        e match {
          case List(1, 2, 3) if guard => println("0")
          case Some(x)                => println("1")
          case List(1, 2, 3)          => println("2")
          case List(1, 2)             => println("3")
          case 1                      => println("4")
          case 2                      => println("5")
          case x :: xs if guard       => println("6")
          case Nil                    => println("7")
          case 2 if guard             => println("8")
          case _: Int                 => println("9")
          case 3                      => println("10")
          case Some(x)                => println("11")
          case None                   => println("12")
        }
      } catch {
        case e: MatchError => println(e)
      }

      println("match4")
      match4(0)
      match4(1)
      match4(2)
      match4(3)
      match4(4)
      match4(List(1, 2, 3))
      match4(List(1, 2, 3), false)
      match4(List(1, 2))
      match4(List(3, 4, 5))
      match4(Nil)
      match4(Some(1))
      match4(Some(1), false)
      match4(Some(1))
      match4("abc")
  }

}
