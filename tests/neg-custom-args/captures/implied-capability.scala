import caps.*

class C1 extends SharedCapability:
  var x: Int = 0 // error

class Ref extends Mutable

class C2 extends SharedCapability: // error
  val r: Ref = Ref()
