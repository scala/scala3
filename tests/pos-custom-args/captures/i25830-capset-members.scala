import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

// Capture-set type member of a class appears in the inferred tpt of a
// polymorphic lambda inside the class. `{C}` here is a TypeRef to the
// class's type member — it must be preserved through PostTyper's
// CleanupRetains via the owner-ancestry branch. See i25830.
class Box[X^](val x0: File^{X}):
  type C^ = X
  val mk = { [D^] => (xs: List[File^{C}]) => xs.head }

def useBox() =
  val a = File(); val b = File()
  val bx = new Box[{a}](a)
  val _ : File^{a} = bx.mk[{b}](List[File^{a}](a))
