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

// Captured and fixed (ordinary, non-captured) refinements mixed freely
trait SomeTrait:
  type Concrete
  type Abstract

type MT2[x <: SomeTrait] = x match
  case SomeTrait { type Concrete = Int; type Abstract = a } => a

class ConcreteIntTrait extends SomeTrait:
  type Concrete = Int
  type Abstract = Boolean

val _: MT2[ConcreteIntTrait] = true

// A captured name shadows a same-named type from the enclosing scope
// instead of being resolved against it (matches the pre-existing var-pattern
// behavior for type-argument position, e.g. `case List[c] => c`).
type c = Int

type ShadowElem[X] = X match
  case List[c] => c

val _: ShadowElem[List[Boolean]] = true // `c` is captured fresh as Boolean, not type-tested against `c = Int`

type ShadowMT[x <: X] = x match
  case X { type A = c } => c

class MyXBoolean extends X:
  type A = Boolean

val _: ShadowMT[MyXBoolean] = true // same shadowing behavior for refinement captures
