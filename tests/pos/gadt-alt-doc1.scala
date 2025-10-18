trait Document[Doc <: Document[Doc]]
sealed trait Conversion[Doc, V]

case class C[Doc <: Document[Doc]]() extends Conversion[Doc, Doc]

def Test[Doc <: Document[Doc], V](conversion: Conversion[Doc, V]) =
  conversion match
    case C() | C() => ??? // error
