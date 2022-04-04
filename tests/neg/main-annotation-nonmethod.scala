object myProgram:

  @main val n = 2 // error

  @main class A // error

  @main val f = ((s: String) => println(s)) // error

end myProgram
