// SIP-80: relative ADT scoping in varargs positions.
// Each arg in a `T*` parameter is typed with element type `T`, so
// `.X` resolves against `T`'s companion just like in non-varargs args.
import scala.language.experimental.targetTypedCompanionShorthand

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
  val n1: Int = palette(.Red)

  // Several args.
  val n3: Int = palette(.Red, .Blue, .Green)

  // Mixed: explicit and relative.
  val nm: Int = palette(Color.Red, .Blue)

  // Splat from a `Seq[Color]` with relative selectors. The val's declared type
  // `Seq[Color]` constrains `Seq.apply`'s type parameter to `Color`, which lets
  // each `.X` resolve against `Color`'s companion.
  val seq: Seq[Color] = Seq(.Red, .Blue)
  val ns: Int = palette(seq*)

  // Repeated parameter after a regular parameter.
  def labelled(name: String, colors: Color*): Int = colors.size
  val nl: Int = labelled("primary", .Red, .Blue, .Green)

  // Varargs in extractor (pattern position) — repeated patterns get the
  // element type as expected type. The scrutinee has known element type, so
  // no inference is needed here.
  val list: List[Color] = List(.Red, .Blue, .Green)
  val s: String = list match
    case List(.Red, _*) => "starts with red"
    case _              => "other"
