// Test that we correctly handle nullable unions when widening.
// We keep nullable after widening.
class Test {
  class A
  class B
  class C extends A

  locally {
    val x: String|Null = ???
    val y = x // String|Null is inferred, this used to crash the compiler
  }

  locally {
    val x: (Int | Null) | String = ???
    val y = x
    val _: Any = y
  }

  locally {
    val x: (A | Null) | B = ???
    val y = x
    val _: AnyRef | Null = y
  }

  locally {
    val x: A | (Null | C) = ???
    val y = x // after simplification before widenUnion, the type of x would become A | Null
    val _: A | Null = y
  }

  locally {
    val x: (A | Null) | (Null | B) = ???
    val y = x
    val _: AnyRef | Null = y
  }

  locally {
    val x: (A | Null) & (B | Null) = ???
    val y = x
    val _: (A & B) | Null = y
  }

  def test1(s: String): String =
    val ss = if !s.isEmpty() then s.trim() else s
    ss + "!"

  def test2(s: String): String =
    val ss = if !s.isEmpty() then s.trim().nn else s
    ss + "!"

  def test3(s: String): String =
    val ss: String = if !s.isEmpty() then s.trim().nn else s
    ss + "!"
}
