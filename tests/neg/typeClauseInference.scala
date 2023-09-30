import scala.language.experimental.typeClauseInference

val notInScopeInferred: [T] => T => T = x => (x: T) // error

def bar[A]: A => A = x => x
val barf1: [T] => T => T = bar(_) // ok
val barf2: [T] => T => T = bar // error, unlike in the original SIP-49.
