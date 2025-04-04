import language.experimental.captureChecking
import language.experimental.modularity
import caps.*

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  val z: Any^ = ???
  def onlyWithZ[cap C](using c: Contains[C, z.type]) = ???

  trait IncludesZ[cap C]:
    val c: Contains[C, z.type]

  trait Foo:
    cap type C >: {x} <: {x,y,z} : IncludesZ

  val foo: Foo = ???
/*   new Foo {
    override given IncludesZ[C]: // FIXME: doesn't work yet
      val c: Contains[C, z.type] = summon
    cap type C = {x,z}
  } */
  onlyWithZ(using foo.C.c)
  onlyWithZ[{z}]
  onlyWithZ[{x,z}]
  onlyWithZ[{x,y,z}]
  onlyWithZ[{x,y}] // error