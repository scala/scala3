import language.experimental.captureChecking
import language.experimental.separationChecking

// `consume` of a separately compiled by-name parameter is enforced
def test(consume r: Ref^, consume s: Ref^) =
  fork(r.set(1))       // ok
  r.set(2)             // error
  forkExcept(s.set(1)) // ok
  s.set(2)             // error
