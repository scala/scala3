import scala.annotation.newMain

object myProgram:

  @newMain @newMain def add1(num: Int, inc: Int): Unit = // error
    println(s"$num + $inc = ${num + inc}")

end myProgram
