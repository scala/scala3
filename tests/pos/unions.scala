object Test:

  def test =
    val x = if ??? then "" else 1
    val _: String | Int = x

object Test2:
  transparent class A
  class B extends A
  class C extends A
  val x = if ??? then B() else C()
  val _: B | C = x

object Test3:
  class A
  class B extends A
  class C extends A
  val x = if ??? then B() else C()
  val _: A = x




