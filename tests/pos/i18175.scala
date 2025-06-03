import scala.compiletime.ops.int.{ +, -, Max }
import scala.compiletime.ops.string.{ Substring, Length, Matches, CharAt }

class Regex[P] private() extends Serializable:
  def unapply(s: CharSequence)(implicit n: Regex.Sanitizer[P]): Option[P] = ???

object Regex:
  def apply[R <: String & Singleton](regex: R): Regex[Compile[R]] = ???

  abstract class Sanitizer[T]
  object Sanitizer:
    given Sanitizer[EmptyTuple] = ???
    given stringcase: [T <: Tuple: Sanitizer] => Sanitizer[String *: T] = ???
    given optioncase: [T <: Tuple: Sanitizer] => Sanitizer[Option[String] *: T] = ???
    given Sanitizer[String] = ???
    given Sanitizer[Option[String]] = ???

  type Compile[R <: String] = Matches["", R] match
    case _ => Reverse[EmptyTuple, Loop[R, 0, Length[R], EmptyTuple, IsPiped[R, 0, Length[R], 0]]]

  type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple, Opt <: Int] <: Tuple = Lo match
    case Hi => Acc
    case _  => CharAt[R, Lo] match
      case '\\' => CharAt[R, Lo + 1] match
        case 'Q' => Loop[R, ToClosingQE[R, Lo + 2], Hi, Acc, Opt]
        case _ => Loop[R, Lo + 2, Hi, Acc, Opt]
      case '[' => Loop[R, ToClosingBracket[R, Lo + 1, 0], Hi, Acc, Opt]
      case ')' => Loop[R, Lo + 1, Hi, Acc, Max[0, Opt - 1]]
      case '(' => Opt match
        case 0 => IsMarked[R, ToClosingParenthesis[R, Lo + 1, 0], Hi] match
          case true => IsCapturing[R, Lo + 1] match
            case false => Loop[R, Lo + 1, Hi, Acc, 1]
            case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, 1]
          case false => IsCapturing[R, Lo + 1] match
            case false => Loop[R, Lo + 1, Hi, Acc, IsPiped[R, Lo + 1, Hi, 0]]
            case true  => Loop[R, Lo + 1, Hi, String *: Acc, IsPiped[R, Lo + 1, Hi, 0]]
        case _ => IsCapturing[R, Lo + 1] match
          case false => Loop[R, Lo + 1, Hi, Acc, Opt + 1]
          case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, Opt + 1]
      case _ => Loop[R, Lo + 1, Hi, Acc, Opt]

  type IsCapturing[R <: String, At <: Int] <: Boolean = CharAt[R, At] match
    case '?' => CharAt[R, At + 1] match
      case '<' => CharAt[R, At + 2] match
        case '=' | '!' => false
        case _ => true
      case _ => false
    case _ => true

  type IsMarked[R <: String, At <: Int, Hi <: Int] <: Boolean = At match
    case Hi => false
    case _ => CharAt[R, At] match
      case '?' | '*' => true
      case '{' => CharAt[R, At + 1] match
        case '0' => true
        case _ => false
      case _ => false

  type IsPiped[R <: String, At <: Int, Hi <: Int, Lvl <: Int] <: Int = At match
    case Hi => 0
    case _ => CharAt[R, At] match
      case '\\' => CharAt[R, At + 1] match
        case 'Q' => IsPiped[R, ToClosingQE[R, At + 2], Hi, Lvl]
        case _ => IsPiped[R, At + 2, Hi, Lvl]
      case '[' => IsPiped[R, ToClosingBracket[R, At + 1, 0], Hi, Lvl]
      case '(' => IsPiped[R, ToClosingParenthesis[R, At + 1, 0], Hi, Lvl + 1]
      case '|' => 1
      case ')' => 0
      case _ => IsPiped[R, At + 1, Hi, Lvl]

  type ToClosingParenthesis[R <: String, At <: Int, Lvl <: Int] <: Int = CharAt[R, At] match
    case '\\' => CharAt[R, At + 1] match
      case 'Q' => ToClosingParenthesis[R, ToClosingQE[R, At + 2], Lvl]
      case _ => ToClosingParenthesis[R, At + 2, Lvl]
    case '[' => ToClosingParenthesis[R, ToClosingBracket[R, At + 1, 0], Lvl]
    case ')' => Lvl match
      case 0 => At + 1
      case _ => ToClosingParenthesis[R, At + 1, Lvl - 1]
    case '(' => ToClosingParenthesis[R, At + 1, Lvl + 1]
    case _   => ToClosingParenthesis[R, At + 1, Lvl]

  type ToClosingBracket[R <: String, At <: Int, Lvl <: Int] <: Int = CharAt[R, At] match
    case '\\' => CharAt[R, At + 1] match
      case 'Q' => ToClosingBracket[R, ToClosingQE[R, At + 2], Lvl]
      case _ => ToClosingBracket[R, At + 2, Lvl]
    case '[' => ToClosingBracket[R, At + 1, Lvl + 1]
    case ']' => Lvl match
      case 0 => At + 1
      case _ => ToClosingBracket[R, At + 1, Lvl - 1]
    case _ => ToClosingBracket[R, At + 1, Lvl]

  type ToClosingQE[R <: String, At <: Int] <: Int = CharAt[R, At] match
    case '\\' => CharAt[R, At + 1] match
      case 'E' => At + 2
      case _ => ToClosingQE[R, At + 2]
    case _ => ToClosingQE[R, At + 1]

  type Reverse[Acc <: Tuple, X <: Tuple] <: Tuple = X match
    case x *: xs    => Reverse[x *: Acc, xs]
    case EmptyTuple => Acc

object Test:
  def main(args: Array[String]): Unit =
    val r75 = Regex("(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*")
    "xyzQzWlongishoverblownW" match
      case r75((Some(g0), Some(g1))) => ??? // failure
