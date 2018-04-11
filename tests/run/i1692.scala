class VCInt(val x: Int) extends AnyVal
class VCString(val x: String) extends AnyVal

class LazyNullable(a: => Int) {
  lazy val l0 = a // null out a

  private val b = "B"
  lazy val l1 = b // null out b

  private val c = "C"
  @volatile lazy val l2 = c // null out c

  private val d = "D"
  lazy val l3 = d + d // null out d (Scalac require single use?)
}

object LazyNullable2 {
  private val a = "A"
  lazy val l0 = a // null out a
}

class LazyNotNullable {
  private val a = 'A'.toInt // not nullable type
  lazy val l0 = a

  private val b = new VCInt('B'.toInt) // not nullable type
  lazy val l1 = b

  private val c = new VCString("C") // should be nullable but is not??
  lazy val l2 = c

  private lazy val d = "D" // not nullable because lazy
  lazy val l3 = d

  val e = "E" // not nullable because not private
  lazy val l4 = e

  private val f = "F" // not nullable because used in mutiple lazy vals
  lazy val l5 = f
  lazy val l6 = f

  private val g = "G" // not nullable because used outside a lazy val initializer
  def foo = g
  lazy val l7 = g

  private val h = "H" // not nullable because field and lazy val not defined in the same class
  class Inner {
    lazy val l8 = h
  }
}

trait LazyTrait {
  private val a = "A"
  lazy val l0 = a
}

object Test {
  def main(args: Array[String]): Unit = {
    nullableTests()
    notNullableTests()
  }

  def nullableTests() = {
    val lz = new LazyNullable('A'.toInt)

    def assertNull(fieldName: String) = {
      val value = readField(fieldName, lz)
      assert(value == null, s"$fieldName was $value, null expected")
    }

    assert(lz.l0 == 'A'.toInt)
    assertNull("a")

    assert(lz.l1 == "B")
    assertNull("b")

    assert(lz.l2 == "C")
    assertNull("c")

    assert(lz.l3 == "DD")
    assertNull("d")

    assert(LazyNullable2.l0 == "A")
    assert(readField("a", LazyNullable2) == null)
  }

  def notNullableTests() = {
    val lz = new LazyNotNullable

    def assertNotNull(fieldName: String) = {
      val value = readField(fieldName, lz)
      assert(value != null, s"$fieldName was null")
    }

    assert(lz.l0 == 'A'.toInt)
    assertNotNull("a")

    assert(lz.l1 == new VCInt('B'.toInt))
    assertNotNull("b")

    assert(lz.l2 == new VCString("C"))
    assertNotNull("c")

    assert(lz.l3 == "D")

    assert(lz.l4 == "E")
    assertNotNull("e")

    assert(lz.l5 == "F")
    assert(lz.l6 == "F")
    assertNotNull("f")

    assert(lz.l7 == "G")
    assertNotNull("g")

    val inner = new lz.Inner
    assert(inner.l8 == "H")
    assertNotNull("h")

    val fromTrait = new LazyTrait {}
    assert(fromTrait.l0 == "A")
    assert(readField("LazyTrait$$a", fromTrait) != null) // fragile: test will break if compiler generated name change
  }

  def readField(fieldName: String, target: Any): Any = {
    val field = target.getClass.getDeclaredField(fieldName)
    field.setAccessible(true)
    field.get(target)
  }
}
