import scala.compiletime.uninitialized

object Test:
  def main(args: Array[String]): Unit =
    val foo = new Foo
    assertEquals(0, foo.int)
    assertEquals(false, foo.bool)
    assertEquals(null, foo.str)
    assertEquals((), foo.unit)
    assertEquals(ValueClass(0), foo.vc)
  end main

  def assertEquals(expected: Any, actual: Any): Unit =
    assert(expected == actual)

  class Foo:
    var int: Int = uninitialized
    var bool: Boolean = uninitialized
    var str: String = uninitialized
    var unit: Unit = uninitialized
    var vc: ValueClass = uninitialized
  end Foo

  class ValueClass(val i: Int) extends AnyVal
end Test
