//> using options -Yrequire-targetName -Werror

// Compile with -strict -Xfatal-warnings -deprecation
import scala.annotation.targetName
class & {   // error

  @targetName("op") def *(x: Int): Int = ???  // OK
  def / (x: Int): Int       // warn
  val frozen_& : Int = ???  // warn
  object some_???           // warn
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
