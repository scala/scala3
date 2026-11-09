import caps.*

class Ref extends Mutable
class One extends ExclusiveCapability
class File

def test() =

  val f1 = () => Ref()
  val _: () -> Ref^ = f1 // error
  val _: () => Ref^ = f1 // error
  val _: () -> Ref^ = () => Ref() // error, because any propagates to function
  val _: () => Ref^ = () => Ref() // ok

  val _: () -> Ref^{fresh} = () => Ref() // ok

  def withFile[T](op: File^ => T): T = ???
  val _ = withFile(f => () => Ref()) // ok
  val _ = withFile(f => Ref())  // ok

  def withPureFile[T](op: File^ -> T): T = ???
  val _ = withPureFile(f => () => Ref()) // error, because any propagates to function
  val _ = withPureFile(f => Ref())  // error, because any propagates to function

  val _: () => Ref^{fresh} = () => Ref() // ok
  val _: () => Ref^{fresh} = f1 // ok

  val f2 = () => One()
  val _: () -> One^ = f2 // error, need fresh
  val _: () => One^ = f2 // error, need fresh
  val _: () => One^ = () => One() // error, need fresh

  val _: () -> One^{fresh} = () => One() // ok
  val _: () => One^{fresh} = () => One() // ok
  val _: () => One^{fresh} = f2 // ok
