object Test extends App {

  def foo() = {
    def f: Int = 1
    val x = f _ // error: not a function: => Int(f)
    x
  }

  def bar(g: => Int) = {
    g _ // error: not a function: => Int(g)
  }

  Console.println((bar{ Console.println("g called"); 42 })()) // error: method bar in object Test$ does not take more parameters
  Console.println(foo()()) // error: method foo in object Test$ does not take more parameters
}
