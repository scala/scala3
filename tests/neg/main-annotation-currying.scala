object myProgram:

  @main def add(num: Int)(inc: Int): Unit = // error
    println(s"$num + $inc = ${num + inc}")

end myProgram
