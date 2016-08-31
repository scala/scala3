
object Test {
  def main(args: Array[String]): Unit = {
    val guard = true
    (1) match {
//      case 0 => println("0")
//      case 1 => println("1")
//      case 2 if guard => println("2")
//      case 2 => println("3")
//      case 3 => println("4")
//      case 4 => println("5")
      case 5 if guard => println("6")
//      case _ if guard => println("7")
      case _ => println("8")
    }

//    (1: Any) match {
      //          case List(1, 2, 3) if guard => println("0")
      //          case Some(x) => println("1")
      //          case List(1, 2, 3) => println("2")
      //          case List(1, 2) => println("3")
      //          case 1 => println("4")
      //          case 2 => println("5")
      //          case x :: xs if guard => println("6")
      //          case Nil => println("7")
//      case 2 if true => 8
//      case _: Int => 9
      //          case 3 => println("10")
      //          case Some(x) => println("11")
      //          case None => println("12")
//    }

//    (1: Any) match {
//                case List(1, 2, 3) => println("0")
      //          case Some(x) => println("1")
//                case List(1, 2, 3) => println("2")
      //          case List(1, 2) => println("3")
      //          case 1 => println("4")
      //          case 2 => println("5")
      //          case x :: xs if guard => println("6")
      //          case Nil => println("7")
//      case 2 if true => 8
//      case _: Int => 9
      //          case 3 => println("10")
      //          case Some(x) => println("11")
      //          case None => println("12")
//    }
////val guard = true
//    (1) match {
////      case List(a, b, c) => 4// 1
////      case List(3, 4, 5) => 5// 2
////      case Nil => // 2
////      case List(3, 4, 5) => // 2
////      case List(3, 4, 5) => // 2
////      case x :: xs => // 2
////      case Nil if true => 8
////      case _: List[AnyRef] if true => 3
////      case _: List[AnyRef] => 4
////      case _: String if true => 5
////      case _: Some => 6
////      case _: String => 7
//
////      case 6 if false     => 2// 3
////      case 6 if guard => 3// 3
////
////      case 8 =>       7      // 4
////      case 2 if true =>   5           // 4
////      case _ if false =>       33       // 5
////      case 2 =>   8          // 4
////      case n: Int if true =>       45       // 5
////      case n: Int =>       46       // 5
////      case n: Int if true =>       44       // 5
//      case _ =>   1          // 4
//
////
////      case List(3, 6, 5) => 5// 2
////      case 3 =>       6      // 4
////      case 7 =>       86      // 4
////      case 5 if true =>       84      // 4
////      case n2 =>       44       // 5
////      case 3 => 2
////      case 3L =>    4         // 4
////      case Some(a) if a == "a" => 3// 4
////      case None          =>4
////      case 2L =>     4        // 4
////      case List(a, b, c) =>4 // 1
////      case 4 =>        4     // 4
////      case _ =>    4         // 4
////      case 4L =>             // 4
////      case 1L =>             // 4
////      case 4 if true =>             // 4
////      case 4 if false =>             // 4
////      case _: Int =>4
////      case _ => 1
////      case Some(a) if a == "b" => // 4
////      case Some(a) if a == "a" => // 4
////      case _ if true =>
////      case Some(1)  => // 4
////      case Some(a)       => // 4
////      case None          =>
////      case n1: Int if true =>             // 5
////      case n2: Int if false =>             // 5
////      case _: Int =>    44         // 5
////      case _ =>       33       // 5
//    }
  }
}
