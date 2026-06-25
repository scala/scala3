// Test that match types can use inline refinement type captures
// instead of requiring a separate type alias

trait X:
  type A

// Old style: requires a type alias
type XOf[a] = X { type A = a }
type MT_old[x <: X] = x match
  case XOf[a] => a

// New style: inline refinement capture
type MT[x <: X] = x match
  case X { type A = a } => a

// Verify both produce the same result
class MyX extends X:
  type A = String

val x: MyX = new MyX
val _: MT[MyX] = "hello"
val _: MT_old[MyX] = "hello"

// Another example from the docs:
trait Base:
  type Value

type ExtractValue[B <: Base] = B match
  case Base { type Value = v } => v

class MyBase extends Base:
  type Value = Int

val _: ExtractValue[MyBase] = 42
