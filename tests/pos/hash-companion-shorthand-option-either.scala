// SIP-80: when the target type is `Option[T]` or `Either[L, T]` and the
// constructor (`Some`, `Right`, `Left`) is applied to a relative selection,
// the type parameter is inferred from the surrounding expected type and
// the shorthand resolves against `T`'s companion.
import scala.language.experimental.hashCompanionShorthand

object OptionEitherUse:

  sealed trait Color
  object Color:
    case object Red   extends Color
    case object Blue  extends Color
    case object Green extends Color

  // Wrapping `#X` in `Some` — pt of the val is `Option[Color]`, which lets
  // type inference for `Some.apply[A]` pick `A = Color`, after which `#Red`
  // is typed against `Color`.
  val o1: Option[Color] = Some(#Red)
  val o2: Option[Color] = Some(#Blue)
  val o3: Option[Color] = None

  // Argument position: the parameter type drives inference.
  def use(o: Option[Color]): Color = o.getOrElse(Color.Red)
  val u1: Color = use(Some(#Green))
  val u2: Color = use(None)

  // Either with a shorthand on the Right side.
  val e1: Either[String, Color] = Right(#Red)
  val e2: Either[Color, Int]    = Left(#Blue)
  val e3: Either[Color, Color]  = Right(#Green)  // exercises both type params

  // Pattern matching reads the same way.
  val s: String = (o1: Option[Color]) match
    case Some(#Red)   => "r"
    case Some(#Blue)  => "b"
    case Some(#Green) => "g"
    case None         => "n"

  // Direct member of the Option / Either companion is also reachable.
  val o4: Option[Int] = #empty       // Option.empty
  val e4: Either[String, Int] = #cond(true, 1, "x")  // Either.cond
