class C1
class C2 extends C1
class C3 extends C2
class C4 extends C3

object Test {
  val c11 : { type T >: C1 <: C1 } = ???
  val c21 : { type T >: C2 <: C1 } = ???
  val c22 : { type T >: C2 <: C2 } = ???
  val c31 : { type T >: C3 <: C1 } = ???
  val c32 : { type T >: C3 <: C2 } = ???
  val c33 : { type T >: C3 <: C3 } = ???
  val c41 : { type T >: C4 <: C1 } = ???
  val c42 : { type T >: C4 <: C2 } = ???
  val c43 : { type T >: C4 <: C3 } = ???
  val c44 : { type T >: C4 <: C4 } = ???

  // Test DOT rule TDECL-<:
  // This is different from scalac
  def test_tdecl_lt_colon(): Unit = {
    implicitly[c31.T <:< c41.T]
    implicitly[c42.T <:< c41.T]
  }

  // Test DOT rule TSEL-<:
  def test_tsel_lt_colon(): Unit = {
    implicitly[c42.T <:< c11.T]
    implicitly[c42.T <:< c21.T]
    implicitly[c42.T <:< c22.T]
  }

  // Test DOT rule <:-TSEL
  def test_lt_colon_tsel(): Unit = {
    implicitly[c33.T <:< c32.T]
    implicitly[c43.T <:< c32.T]
    implicitly[c44.T <:< c32.T]
  }
}
