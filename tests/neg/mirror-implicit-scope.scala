import scala.deriving._

object Test {
  class SomeClass
  case class ISB(i: Int, s: String, b: Boolean)
  case class BI(b: Boolean, i: Int)

  val v0 = the[Mirror.ProductOf[ISB]] // OK
  val v1 = the[SomeClass & Mirror.ProductOf[ISB]] // error
  val v2 = the[Mirror.ProductOf[ISB] & Mirror.ProductOf[BI]] // error
  val v3 = the[Mirror.Product { type MirroredType = ISB ; def foo: Int }] // error
  val v4 = the[Mirror.Product { type MirroredType = ISB ; def foo(i: Int): Int }] // error
  val v5 = the[Mirror.Product { type MirroredType = ISB ; def foo[T](t: T): T }] // error // error
  val v6 = the[Mirror.Product { type MirroredType = ISB ; val foo: Int }] // error
}
