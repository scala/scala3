package hello

enum Color with
  case Red, Green, Blue

object HelloWorld with
  def main(args: Array[String]): Unit = {
    println("hello dotty.superbootstrapped!")
    println(Color.Red)
    println(Color.Green)
    println(Color.Blue)
  }
