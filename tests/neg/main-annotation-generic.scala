import scala.annotation.newMain

object myProgram:

  @newMain def nop[T](t: T): T = // error
    t

end myProgram
