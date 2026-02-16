
trait Scope
object Scope:
  given i: Int = ???

type ReferencesScope[S] >: Int <: Int

type ScopeToInt[Why] = Why match
  case Scope => Int

def foo[T](using d: ReferencesScope[T]): Any = ???

def bar[T](using d: ScopeToInt[T]): Any = ???
