import caps.*

class Ref extends Mutable
class One extends ExclusiveCapability
class File

def test() =

  val _: () -> Ref^ = () => Ref() // error, monotonicity violation
  val _: () => Ref^ = () => Ref() // ok
  val _: () -> () => Ref^ = () => () => Ref() // error, monotonicity violation
  val _: () => () => Ref^ = () => () => Ref() // ok

  val f1 = () => Ref()

  val _: () -> Ref^ = f1 // error
  val _: () => Ref^ = f1 // error
