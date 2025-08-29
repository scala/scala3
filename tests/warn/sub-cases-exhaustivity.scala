import scala.language.experimental.subCases

enum E:
  case A(e: E)
  case B(e: E)
  case C

  def f: E = ???
end E
import E.*

object Test:
  val e: E = ???

  e match // warn: match may not be exhaustive: It would fail on pattern case: E.A(_) | E.B(_)
    case A(e1) if e1.f match
      case B(_) => 11
      case C => 12
    case B(e1) if e1.f match
      case C => 21
      case A(_) => 22
    case C => 3

  e match // warn: match may not be exhaustive: It would fail on pattern case: E.B(_)
    case A(e1) if e1.f match
      case B(_) => 11
      case C => 12
    case B(e1) if e1.f match
      case C => 21
      case A(_) => 22
    case A(_) => 3 // nowarn: should not be reported as unreachable
    case A(_) => 3 // warn: unreacheable
    case C => 4
