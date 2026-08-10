object Test:
  lazy val (a, _, b) = ({println("a"); 1}, {println("c"); 3}, {println("b"); 2})
  
  def main(args: Array[String]): Unit =
    println(a)
    println(b)
    println(a)
    println(b)
