object Test:
  scala.Console.println("hello") // error

  def f() =
    scala.Console.println("hello") // error

  val g = () =>
    scala.Console.println("hello")

