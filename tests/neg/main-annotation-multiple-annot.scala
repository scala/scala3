object myProgram:

  @main @main def add1(num: Int, inc: Int): Unit = // error
    println(s"$num + $inc = ${num + inc}")

end myProgram
