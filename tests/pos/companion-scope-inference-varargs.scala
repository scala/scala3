// SIP-80: bare-ident varargs.
import scala.language.experimental.companionScopeInference

sealed trait Color
object Color:
  case object Red   extends Color
  case object Blue  extends Color
  case object Green extends Color

object Vararg:
  def palette(colors: Color*): Int = colors.size

  // 0 args.
  val n0: Int = palette()

  // Single arg.
  val n1: Int = palette(Red)

  // Several args.
  val n3: Int = palette(Red, Blue, Green)

  // Mixed: explicit and inferred.
  val nm: Int = palette(Color.Red, Blue)

  // Splat from a `Seq[Color]`. The val's declared type constrains
  // `Seq.apply`'s type parameter to `Color`.
  val seq: Seq[Color] = Seq(Red, Blue)
  val ns: Int = palette(seq*)

  // Repeated parameter after a regular parameter.
  def labelled(name: String, colors: Color*): Int = colors.size
  val nl: Int = labelled("primary", Red, Blue, Green)

  // Varargs in extractor (pattern position).
  val list: List[Color] = List(Red, Blue, Green)
  val s: String = list match
    case List(Red, _*) => "starts with red"
    case _             => "other"
