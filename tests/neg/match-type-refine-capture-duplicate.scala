// Each captured name must appear exactly once in the whole case pattern,
// same as for any other pattern-bound type variable.

trait KV:
  type Key
  type Value

// Same captured name used for two different members of the same refinement
type Dup1[x <: KV] = x match
  case KV { type Key = a; type Value = a } => a // error

trait Y:
  type A

// Same captured name used once via a refinement capture and once via an
// ordinary type-arg var-pattern elsewhere in the pattern
type Dup2[p] = p match
  case (List[a], Y { type A = a }) => a // error
