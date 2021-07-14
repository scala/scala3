package example

def m1(a: Int { val x: Int }) = ???
def m2(x: { val x: Int; def y: Int }) = ???
def m3(x: { val x: Int; def y: Int; type z }) = ???
trait PolyHolder {
  def foo[T](t: T): Any
}

def m4(x: PolyHolder { def foo[T](t: T): T }) = ???
def m5[Z](x: Int): PolyHolder { def foo[T](t: T): T } = ???

type m6 = [X] =>> PolyHolder { def foo[T](t: T): T }

class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

type Person = Record {
  val name: String
  val age: Int
}

// RecType
class C { type T1; type T2 }
type C2 = C { type T1; type T2 = T1 }

trait SpecialRefinement {
  def pickOne(as: String*): Option[Any]
}

class PickOneRefinement_1[S <: SpecialRefinement { def pickOne(as: String*): Option[String] }] {
  def run(s: S, as: String*): Option[String] = s.pickOne(as:_*)
}
