import scala.annotation.threadUnsafe

class VCInt(val x: Int) extends AnyVal
class VCString(val x: String) extends AnyVal

class LazyNullable(a: => Int) {
  @threadUnsafe lazy val l0 = a // null out a

  private[this] val b = "B"
  @threadUnsafe lazy val l1 = b // null out b

  private[this] val c = "C"
  @threadUnsafe lazy val l2 = c // null out c

  private[this] val d = "D"
  @threadUnsafe lazy val l3 = d + d // null out d (Scalac require single use?)

  private [this] val e = "E"
  @threadUnsafe lazy val l4 = try e finally () // null out e

  private[this] val i = "I"
  // null out i even though the try ends up lifted, because the LazyVals phase runs before the LiftTry phase
  @threadUnsafe lazy val l5 = try i catch { case e: Exception => () }
}

object LazyNullable2 {
  private[this] val a = "A"
  @threadUnsafe lazy val l0 = a // null out a
}

class LazyNotNullable {
  private[this] val a = 'A'.toInt // not nullable type
  @threadUnsafe lazy val l0 = a

  private[this] val b = new VCInt('B'.toInt) // not nullable type
  @threadUnsafe lazy val l1 = b

  private[this] val c = new VCString("C") // should be nullable but is not??
  @threadUnsafe lazy val l2 = c

  @threadUnsafe private[this]  lazy val d = "D" // not nullable because lazy
  @threadUnsafe lazy val l3 = d

  private val e = "E" // not nullable because not private[this]
  @threadUnsafe lazy val l4 = e

  private[this] val f = "F" // not nullable because used in mutiple @threadUnsafe lazy vals
  @threadUnsafe lazy val l5 = f
  @threadUnsafe lazy val l6 = f

  private[this] val g = "G" // not nullable because used outside a @threadUnsafe lazy val initializer
  def foo = g
  @threadUnsafe lazy val l7 = g

  private[this] val h = "H" // not nullable because field and @threadUnsafe lazy val not defined in the same class
  class Inner {
    @threadUnsafe lazy val l8 = h
  }

}

trait LazyTrait {
  private val a = "A"
  @threadUnsafe lazy val l0 = a
}

class Foo(val x: String)

class LazyNotNullable2(x: String) extends Foo(x) {
  @threadUnsafe lazy val y = x // not nullable. Here x is super.x
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

    assert(lz.l4 == "E")
    assertNull("e")

    assert(lz.l5 == "I")
    assertNull("i")

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
    assertNotNull("LazyNotNullable$$h") // fragile: test will break if compiler generated names change

    val fromTrait = new LazyTrait {}
    assert(fromTrait.l0 == "A")
    assert(readField("LazyTrait$$a", fromTrait) != null) // fragile: test will break if compiler generated names change

    val lz2 = new LazyNotNullable2("Hello")
    assert(lz2.y == "Hello")
    assert(lz2.x == "Hello")
  }

  def readField(fieldName: String, target: Any): Any = {
    val field = target.getClass.getDeclaredField(fieldName)
    field.setAccessible(true)
    field.get(target)
  }
}
