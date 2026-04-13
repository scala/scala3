package test

object Console:
  val out: java.io.PrintStream^ = System.out
  def println(s: String) = out.println(s)


object Test:// uses Console uses_init Console:
  Console.println("hello")

  def f() =
    Console.println("hello")

