type DiagnosticConsturctor = (Int) => DiagnosticSet

final class Diagnostic

final class DiagnosticSet(elements: List[Diagnostic] = List())

enum Result:
  case Success extends Result
  case Failure(diagnose: DiagnosticConsturctor) extends Result
  def diagnose(n: Int): DiagnosticSet =
    this match
      case Success => DiagnosticSet()
      case Failure(d) => d(n)

@main def Test(): Unit =
  val r : Result = Result.Failure((n) => DiagnosticSet(List(Diagnostic())))
  r.diagnose(1)
