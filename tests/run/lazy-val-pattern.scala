object Test:
  lazy val (a, b) = ({println("a"); 1}, {println("b"); 2})
  
  def main(args: Array[String]): Unit =
    println(a)
    println(b)
    println(a)
    println(b)
