// Compile with -strict -Xfatal-warnings -deprecation
import scala.annotation.alpha
class & {   // error

  @alpha("op") def *(x: Int): Int = ???  // OK
  def / (x: Int): Int       // error
  val frozen_& : Int = ???  // error
  object some_???           // error
}
