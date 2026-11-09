object Test:
  lazy val (a, b) = {println("a"); (1, 2)}
  
  def main(args: Array[String]): Unit =
    println(a)
    println(b)
    println(a)
    println(b)
