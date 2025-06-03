import language.experimental.captureChecking
import caps.*

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  val z: Any^ = ???
  def onlyWithZ[C^](using c: Contains[C, z.type]) = ???

  trait Foo:
    type C >: {z,x} <: {x,y,z}

  val foo: Foo = ???
  onlyWithZ[{foo.C}]
  onlyWithZ[{z}]
  onlyWithZ[{x,z}]
  onlyWithZ[{x,y,z}]
  onlyWithZ[{x,y}] // error