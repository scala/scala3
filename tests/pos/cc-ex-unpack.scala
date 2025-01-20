import language.experimental.captureChecking
import caps.{Exists, Capability}

class C

type EX1 = (c: Exists) -> (C^{c}, C^{c})

type EX2 = () -> (c1: Exists) -> (c2: Exists) -> (C^{c1}, C^{c2})

type EX3 = () -> (c: Exists) -> () -> C^{c}

type EX4 = () -> () -> (c: Exists) -> C^{c}

def Test =
  def f =
    val ex1: EX1 = ???
    val c1 = ex1
    c1
