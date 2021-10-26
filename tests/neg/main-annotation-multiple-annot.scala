class myMain extends main

object myProgram:

  @main @main def add1(num: Int)(inc: Int): Unit = // error
    println(s"$num + $inc = ${num + inc}")

  @myMain @main def add2(num: Int)(inc: Int): Unit = // error
    println(s"$num + $inc = ${num + inc}")

  @myMain @myMain def add3(num: Int)(inc: Int): Unit = // error
    println(s"$num + $inc = ${num + inc}")

end myProgram
