import caps.{Capability, Sharable}

def foo() =
  val x: Sharable = ???

  val z3 =
    if x == null then (y: Unit) => x else (y: Unit) => new Capability() {} // error


