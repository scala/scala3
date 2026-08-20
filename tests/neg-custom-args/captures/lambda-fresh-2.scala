import caps.*

class Ref extends Mutable
class One extends ExclusiveCapability
class Sha extends SharedCapability
class File

def test() =
  val f1: File^ = File()
  val f2: File^ = File()
  val f3: File^ = File()
  val a: () => File^{fresh} = () => f1  // error
  val b: () => File^ = () => f2  // ok
  def g(): File^ = f3  // error

def test1() =
  val f1 = Ref()
  val f2 = Ref()
  val f3 = Ref()
  val a: () => Ref^{fresh} = () => f1  // error
  val b: () => Ref^ = () => f2  // ok
  def g(): Ref^ = f3  // error

def test2() =
  val f1 = One()
  val f2 = One()
  val f3 = One()
  val a: () => One^{fresh} = () => f1  // error
  val b: () => One^ = () => f2  // ok
  def g(): One^ = f3  // error

def test3() =
  val f1 = Sha()
  val f2 = Sha()
  val f3 = Sha()
  val a: () => Sha^{fresh} = () => f1  // ok, SharedCapabilities are excluded from frhsness checking
  val b: () => Sha^ = () => f2  // ok
  def g(): Sha^ = f3  // ok, SharedCapabilities are excluded from frhsness checking
