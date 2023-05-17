package hello

enum Color:
  case Red, Green, Blue

object HelloWorld:
  def main(args: Array[String]): Unit = {
    println("hello 2.13 library bootstrapped")
    println(Color.Red)
    println(Color.Green)
    println(Color.Blue)
  }
