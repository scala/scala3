import scala.annotation.newMain

object myProgram:
  implicit val x: Int = 2
  given Int = 3

  @newMain def showImplicit(implicit num: Int): Unit = // error
    println(num)

  @newMain def showUsing(using num: Int): Unit = // error
    println(num)

end myProgram
