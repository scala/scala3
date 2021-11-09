class myMain extends main

object myProgram:

  @main val n = 2 // error

  @main class A // error

  @main val f = ((s: String) => println(s)) // error

  @myMain val m = 2 // error

  @myMain class B // error

  @myMain val g = ((s: String) => println(s)) // error

end myProgram
