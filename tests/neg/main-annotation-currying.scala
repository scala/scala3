import scala.annotation.newMain

object myProgram:

  @newMain def add(num: Int)(inc: Int): Unit = // error
    println(s"$num + $inc = ${num + inc}")

end myProgram
