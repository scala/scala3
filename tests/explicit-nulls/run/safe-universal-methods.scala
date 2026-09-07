
object Test {
  def toString1(x: Any): String = x.toString()
  def toString2(x: AnyVal): String = x.toString()
  def toString3(x: Matchable): String = x.toString()
  def toString4(x: Int | Null): String = x.toString()
  def toString5(x: List[String] | Null): String = x.toString()
  def toString6(x: Null): String = x.toString()

  def toStringGen1[T <: Any](x: T): String = x.toString()
  def toStringGen2[T <: AnyVal](x: T): String = x.toString()
  def toStringGen3[T <: Matchable](x: T): String = x.toString()
  def toStringGen4[T <: Int | Null](x: T): String = x.toString()
  def toStringGen5[T <: List[String] | Null](x: T): String = x.toString()
  def toStringGen6[T <: Null](x: T): String = x.toString()

  def getClass1(x: Any): Class[?] = x.getClass()
  def getClass2(x: AnyVal): Class[?] = x.getClass()
  def getClass3(x: Matchable): Class[?] = x.getClass()
  def getClass4(x: Int | Null): Class[?] = x.getClass()
  def getClass5(x: List[String] | Null): Class[?] = x.getClass()
  def getClass6(x: Null): Class[?] = x.getClass()

  def getClassGen1[T <: Any](x: T): Class[?] = x.getClass()
  def getClassGen2[T <: AnyVal](x: T): Class[?] = x.getClass()
  def getClassGen3[T <: Matchable](x: T): Class[?] = x.getClass()
  def getClassGen4[T <: Int | Null](x: T): Class[?] = x.getClass()
  def getClassGen5[T <: List[String] | Null](x: T): Class[?] = x.getClass()
  def getClassGen6[T <: Null](x: T): Class[?] = x.getClass()

  def main(args: Array[String]): Unit = {
    assertEquals("List(foo)", toString1(List("foo")))
    assertEquals("5", toString2(5))
    assertEquals("List(foo)", toString3(List("foo")))
    assertEquals("5", toString4(5))
    assertEquals("List(foo)", toString5(List("foo")))

    assertEquals("null", toString1(null))
    assertEquals("null", toString2(null))
    assertEquals("null", toString3(null))
    assertEquals("null", toString4(null))
    assertEquals("null", toString5(null))
    assertEquals("null", toString6(null))

    assertEquals("List(foo)", toStringGen1(List("foo")))
    assertEquals("5", toStringGen2(5))
    assertEquals("List(foo)", toStringGen3(List("foo")))
    assertEquals("5", toStringGen4(5))
    assertEquals("List(foo)", toStringGen5(List("foo")))

    assertEquals("null", toStringGen1(null))
    assertEquals("null", toStringGen2(null))
    assertEquals("null", toStringGen3(null))
    assertEquals("null", toStringGen4(null))
    assertEquals("null", toStringGen5(null))
    assertEquals("null", toStringGen6(null))

    assertEquals(classOf[::[?]], getClass1(List("foo")))
    assertEquals(classOf[Integer], getClass2(5))
    assertEquals(classOf[::[?]], getClass3(List("foo")))
    assertEquals(classOf[Integer], getClass4(5))
    assertEquals(classOf[::[?]], getClass5(List("foo")))

    assertEquals(classOf[Null], getClass1(null))
    assertEquals(classOf[Null], getClass2(null))
    assertEquals(classOf[Null], getClass3(null))
    assertEquals(classOf[Null], getClass4(null))
    assertEquals(classOf[Null], getClass5(null))
    assertEquals(classOf[Null], getClass6(null))

    assertEquals(classOf[::[?]], getClassGen1(List("foo")))
    assertEquals(classOf[Integer], getClassGen2(5))
    assertEquals(classOf[::[?]], getClassGen3(List("foo")))
    assertEquals(classOf[Integer], getClassGen4(5))
    assertEquals(classOf[::[?]], getClassGen5(List("foo")))

    assertEquals(classOf[Null], getClassGen1(null))
    assertEquals(classOf[Null], getClassGen2(null))
    assertEquals(classOf[Null], getClassGen3(null))
    assertEquals(classOf[Null], getClassGen4(null))
    assertEquals(classOf[Null], getClassGen5(null))
    assertEquals(classOf[Null], getClassGen6(null))
  }

  def assertEquals(expected: Any, actual: Any): Unit =
    if actual != expected then
      throw AssertionError(s"expected '$expected' but got '$actual'")
}
