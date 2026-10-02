import scala.language.strictEquality

// Without relaxedNullChecks, null checks on nullable value types are rejected
def flowIf(x: Int | Null): Int =
  if x != null then x else 0 // error

def flowMatch(x: Int | Null): Int =
  x match
    case null => 0 // error
    case y => y
