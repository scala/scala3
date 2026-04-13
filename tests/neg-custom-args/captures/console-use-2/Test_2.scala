package test

object Test:
  Console.println("hello") // error

  def f() =
    Console.println("hello") // error

object Good uses Console uses_init Console:
  Console.println("hello")

  def f() =
    Console.println("hello")



