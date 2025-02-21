// This contains a lot of illegal capture ref errors, which should be treated as
// noise. The problem is that we can't write an existential type by hand,
// sincxe existentially typed variables carry an @existential annotation, which
// can't be written down. The interesting errors are the rest.

import language.experimental.captureChecking
import caps.{Exists, Capability, existential}


class C

type EX1 = () => (c: Exists) => (C^{c}, C^{c}) // error: illegal capture ref

type EX2 = () => (c1: Exists) => (c2: Exists) => (C^{c1}, C^{c2}) // error: illegal capture ref

type EX3 = () => (c: Exists) => (x: Object^) => C^{c} // error: illegal capture ref

type EX4 = () => (x: Object^) => (c: Exists) => C^{c} // error: illegal capture ref

def Test =
  val ex1: EX1 = ??? // error: illegal capture ref
  val ex2: EX2 = ??? // error: illegal capture ref
  val _: EX1 = ex1  // error: illegal capture ref
  val _: EX2 = ex1  // error separation // error: illegal capture ref
  val _: EX1 = ex2  // error: illegal capture ref

  val ex3: EX3 = ???  // error: illegal capture ref
  val ex4: EX4 = ???  // error: illegal capture ref
  val _: EX4 = ex3 // error: illegal capture ref
  val _: EX4 = ex4 // error: illegal capture ref
  val _: EX3 = ex4 // error: type mismatch // error: illegal capture ref
