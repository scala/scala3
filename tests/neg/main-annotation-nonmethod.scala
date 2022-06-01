import scala.annotation.newMain

object myProgram:

  @newMain val n = 2 // error

  @newMain class A // error

  @newMain val f = ((s: String) => println(s)) // error

end myProgram
