package example

def m1(a: Int { val x: Int }) = ???
def m2(x: { val x: Int; def y: Int }) = ???
def m3(x: { val x: Int; def y: Int; type z }) = ???

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
