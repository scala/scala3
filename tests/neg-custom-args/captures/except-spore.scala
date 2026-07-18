import caps.{any, Classifier, SharedCapability, Unscoped}

// "Classifying Capabilities" §6.5: a Spore may only capture Serializable capabilities.
trait Serializable extends Classifier, SharedCapability:
  def serialize(): Array[Byte]
  def deserialize(from: Array[Byte]): Unit

class Spore(inner: () ->{any.only[Serializable]} Unit)

class SerCap extends Serializable:
  def serialize(): Array[Byte] = ???
  def deserialize(from: Array[Byte]): Unit = ???
class UnsCap extends Unscoped   // disjoint from Serializable

def test(s: SerCap^, u: UnsCap^, o: Object^): Unit =
  val a = Spore(() => s.serialize())   // ok
  val b = Spore(() => println(u))      // error: UnsCap is Unscoped
  val c = Spore(() => println(o))      // error: o is unclassified
