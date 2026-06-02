import caps.*

class Ref extends Mutable
class One extends ExclusiveCapability

def test() =
  val f1 = () => Ref()
  val _: () -> Ref^ = f1 // error
  val _: () => Ref^ = f1 // ok
  val _: () => Ref^ = () => Ref() // ok

  val _: () -> Ref^{fresh} = () => Ref() // error, Unscoped are not fresh
  val _: () => Ref^{fresh} = () => Ref() // error, Unscoped are not fresh
  val _: () => Ref^{fresh} = f1 // error, Unscoped are not fresh

  val f2 = () => One()
  val _: () -> One^ = f2 // error, need fresh
  val _: () => One^ = f2 // error, need fresh
  val _: () => One^ = () => One() // error, need fresh

  val _: () -> One^{fresh} = () => One() // ok
  val _: () => One^{fresh} = () => One() // ok
  val _: () => One^{fresh} = f2 // ok
