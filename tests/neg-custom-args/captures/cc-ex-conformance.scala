import language.experimental.captureChecking
import caps.{Exists, Capability}


class C

type EX1 = () => (c: Exists) => (C^{c}, C^{c})

type EX2 = () => (c1: Exists) => (c2: Exists) => (C^{c1}, C^{c2})

type EX3 = () => (c: Exists) => (x: Object^) => C^{c}

type EX4 = () => (x: Object^) => (c: Exists) => C^{c}

def Test =
  val ex1: EX1 = ???
  val ex2: EX2 = ???
  val _: EX1 = ex1
  val _: EX2 = ex1  // error separation
  val _: EX1 = ex2  // ok

  val ex3: EX3 = ???
  val ex4: EX4 = ???
  val _: EX4 = ex3 // ok
  val _: EX4 = ex4 // error (???) Probably since we also introduce existentials on expansion
  val _: EX3 = ex4 // error
