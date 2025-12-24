import scala.language.experimental.subCases

enum E extends Exception:
  case A(x: Any)
  case B(x: Any)
  case C

end E
import E.*

def test(op: => Nothing): String =
  try op catch
    case A(x: Int) if true if x match
      case 1 => "A(1)"
      case 2 => "A(2)"
    case B(x: String) if x match
      case "a" => "B(a)"
      case "b" => "B(b)"
    case _ => "other"
end test

@main def Test =

  def check(e: E, r: String): Unit = assert(test(throw e) == r)

  check(A(1), "A(1)")
  check(A(2), "A(2)")
  check(A(3), "other")
  check(B("b"), "B(b)")
  check(B(1), "other")
