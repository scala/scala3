object Test:
  def main(args: Array[String]): Unit =
    println(Macro.decorate(Greeting.greet()))

