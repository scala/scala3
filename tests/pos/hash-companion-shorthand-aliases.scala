// SIP-80: `#` companion shorthand should work with transparent type aliases,
// `import`-introduced types, and `export`-introduced types.
import scala.language.experimental.hashCompanionShorthand

object Lib:
  sealed trait Color
  object Color:
    case object Red   extends Color
    case object Blue  extends Color
    case object Green extends Color

object AliasUser:
  // Transparent type alias.
  type MyColor = Lib.Color

  def foo(color: MyColor): Unit = ()

  val c1: MyColor = #Red          // resolves through MyColor → Color
  foo(#Blue)
  foo(color = #Green)

  // Match on the alias.
  val s: String = c1 match
    case #Red   => "r"
    case #Blue  => "b"
    case #Green => "g"

object ImportUser:
  // `import` brings the type into scope; resolution still finds the
  // companion via the alias's own prefix.
  import Lib.Color
  def paint(c: Color): Unit = ()
  val c: Color = #Red
  paint(#Blue)

object ExportingFacade:
  // Re-export the type. Clients that import from this facade should still
  // be able to use the shorthand.
  export Lib.Color

object ExportClient:
  import ExportingFacade.Color
  def use(c: Color): Unit = ()
  val c: Color = #Red
  use(#Green)

  // Combined: alias on top of an exported type.
  type Hue = Color
  val h: Hue = #Blue
