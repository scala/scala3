
trait Scope
object Scope:
  given i: Int = ???

type ReferencesScope[S] >: Int <: Int

type ScopeToInt[Why] = Why match
  case Scope => Int

def foo[T](using d: ReferencesScope[T]): Any = ???

def bar[T](using d: ScopeToInt[T]): Any = ???

def test: Unit =
  foo[Scope] // ok
  bar[Scope] // error

  import Scope.i
  bar[Scope] // ok

  /*
  Before the changes:
  `ScopeToInt[Scope]` may or may not be reduced before implicit search,
  thereby impacting the scope considered for the search. `Scope.i` is included
  iff `Scope` still appears in the type, which is the case only before reduction.
  In contrast, `ReferencesScope[Scope]` is ok since it will never lose the anchor.
  */
