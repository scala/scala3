import scala.language.experimental.subCases

enum E:
  case A(e: E)
  case B(e: E)
  case C

  def f: Option[E] = this match
    case A(e) => Some(e)
    case B(e) => Some(e)
    case C => None

end E
import E.*


@main def Test =

  def test(e: E): Int = e match
    case A(B(e1)) if true if e1.f match
      case Some(x) if x match
        case A(_) => 11
        case C => 12
    case B(A(e1)) if e1.f match
      case Some(C) if false || true => 21
      case None => 22
    case _ => 3
  end test

  def check(e: E, r: Int): Unit = assert(test(e) == r)

  check(A(A(C)), 3)

  val x1 = B(A(C))
  check(A(B(x1)), 11)

  val x2 = B(C)
  check(A(B(x2)), 12)

  check(A(B(C)), 3)
  check(A(B(B(B(C)))), 3)

  check(B(A(x2)), 21)
  check(B(A(C)), 22)

  check(A(A(C)), 3)
