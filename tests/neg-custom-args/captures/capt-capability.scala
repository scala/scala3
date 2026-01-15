import caps.{Capability, SharedCapability}

def foo() =
  val x: SharedCapability = ???

  val z3 =
    if x == null then (y: Unit) => x else (y: Unit) => new Capability() {} // error


