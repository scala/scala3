
// This used to succeed with old creator methods scheme
// What happened was: the overloading resolution gave an ambiguous
// overload, but then the falblback picked the constructor
object Test:

  case class Record(elems: (String, Any)*)

  object Record:

    inline def apply[R <: Record](elems: (String, Any)*) : R = new Record(elems*).asInstanceOf[R]

    def fromUntypedTuple(elems: (String, Any)*): Record = Record(elems*) // error: ambiguous overload

  def apply(x: String): Test = Test(x ++ x)

  def apply(x: Integer): Test = Test(x + x)

  Test(null) // error: ambiguous overload

end Test

class Test(x: Object)