import caps.*

class Shared extends SharedCapability

abstract class Mut extends Mutable:
  def get: Int
  update def set(x: Int): Unit

def Test(c: Shared, m: Mut) =
  val f = () => println(c)
  val _: () ->{any.rd} Unit = f // error
  val g = () => m.get
  val _: () ->{any.rd} Int = g // ok

