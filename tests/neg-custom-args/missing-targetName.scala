// Compile with -strict -Xfatal-warnings -deprecation
import scala.annotation.targetName
class & {   // error

  @targetName("op") def *(x: Int): Int = ???  // OK
  def / (x: Int): Int       // error
  val frozen_& : Int = ???  // error
  object some_???           // error
}
