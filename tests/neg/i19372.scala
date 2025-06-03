//> using options -explain-cyclic
object Test1:
  type AAA = List[bar.BBB] // error: cyclic
  def foo: AAA = ???
  object bar:
    opaque type BBB = AAA

object Test2:
  type A = bar.B  // error: cyclic
  def foo: A = ???
  object bar:
    opaque type B = A

object Test3:
  type AAA = List[bar.BBB] // error: cyclic
  def foo: AAA = ???
  object bar:
    type BBB = AAA

object Test4:
  type A = bar.B // error: cyclic
  def foo: A = ???
  object bar:
    type B = A

trait Ptr[T]

object aliases:
  import structs.*
  type UCharIteratorReserved = Ptr[UCharIterator] // error: cyclic
  object UCharIteratorReserved:
    def iterator: UCharIterator = ???

object structs:
  import aliases.{*, given}
  opaque type UCharIterator = Ptr[UCharIteratorReserved]
  object UCharIterator:
    def reservedFn: UCharIteratorReserved = ???