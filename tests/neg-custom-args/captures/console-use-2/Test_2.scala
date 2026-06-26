package test

object Test:
  Console.println("hello") // error

  def f() =
    Console.println("hello") // error

object Good uses Console initially, Console:
  Console.println("hello")

  def f() =
    Console.println("hello")

val _ = Console.println("hello") // error

def f = Console.println("hello") // error