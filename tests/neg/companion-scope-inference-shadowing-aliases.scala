// Shadowing counterpart of `tests/pos/companion-inference-aliases.scala`.
// A wildcard import drags in a same-named value from an unrelated object;
// normal resolution finds it first, and companion scope inference does not fire.
import scala.language.experimental.companionScopeInference

object Lib:
  sealed trait Color
  object Color:
    case object Red  extends Color
    case object Blue extends Color

object Strings:
  val Red = "red"
  val Blue = "blue"

object AliasUser:
  type MyColor = Lib.Color

  // Wildcard import brings `Red`/`Blue` from Strings into scope.
  import Strings.*

  val c: MyColor = Red // error
