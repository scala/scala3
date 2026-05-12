// SIP-80 companion scope inference works with transparent type aliases,
// `import`-introduced types, and `export`-introduced types.
import scala.language.experimental.companionScopeInference

object Lib:
  sealed trait Color
  object Color:
    case object Red   extends Color
    case object Blue  extends Color
    case object Green extends Color

object AliasUser:
  type MyColor = Lib.Color

  def foo(color: MyColor): Unit = ()

  val c1: MyColor = Red          // dealias MyColor → Lib.Color → Lib.Color.Red
  foo(Blue)
  foo(color = Green)

  // Match on the alias.
  val s: String = c1 match
    case Red   => "r"
    case Blue  => "b"
    case Green => "g"

object ImportUser:
  import Lib.Color
  def paint(c: Color): Unit = ()
  val c: Color = Red
  paint(Blue)

object ExportingFacade:
  export Lib.Color

object ExportClient:
  import ExportingFacade.Color
  def use(c: Color): Unit = ()
  val c: Color = Red
  use(Green)

  // Combined: alias on top of an exported type.
  type Hue = Color
  val h: Hue = Blue
