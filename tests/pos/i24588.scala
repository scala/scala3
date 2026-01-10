// Test for issue #24588: Inline match with string interpolation
// The compiler incorrectly marked matched values as erased when using string interpolation
// because StringContext.s is a macro marked with the Erased flag.

object Test:
  // Basic inline match with union type
  inline def dispatch(inline v: Long | String): Unit =
    inline v match
      case str: String => printString(str)
      case long: Long  => printLong(long)

  def printString(s: String) = println(s)
  def printLong(l: Long) = println(l)

  // Test with regular strings (always worked)
  def testRegularStrings(): Unit =
    dispatch("hello")
    dispatch(System.nanoTime().toString)

  // Test with string interpolation (was broken)
  def testStringInterpolation(): Unit =
    dispatch(s"hello")
    dispatch(s"${System.nanoTime()}")
    dispatch(s"${System.nanoTime().toString}")
    dispatch(s"time: ${System.nanoTime()}")
    dispatch(s"${"nested"}")

  // Test with f-interpolator
  def testFInterpolation(): Unit =
    dispatch(f"${3.14}%.2f")
    dispatch(f"${42}%d")

  // Test with raw interpolator
  def testRawInterpolation(): Unit =
    dispatch(raw"hello\nworld")

  // More complex inline match
  inline def complexDispatch(inline v: Int | Long | String | Double): String =
    inline v match
      case i: Int    => s"int: $i"
      case l: Long   => s"long: $l"
      case s: String => s"string: $s"
      case d: Double => s"double: $d"

  def testComplexDispatch(): Unit =
    val r1: String = complexDispatch(42)
    val r2: String = complexDispatch(42L)
    val r3: String = complexDispatch("hello")
    val r4: String = complexDispatch(3.14)
    val r5: String = complexDispatch(s"interpolated")
    val r6: String = complexDispatch(s"${System.nanoTime()}")

  // Inline match returning the matched value
  inline def identity(inline v: Long | String): Long | String =
    inline v match
      case str: String => str
      case long: Long  => long

  def testIdentity(): Unit =
    val s1: Long | String = identity("hello")
    val s2: Long | String = identity(s"interpolated")
    val s3: Long | String = identity(s"${System.nanoTime()}")
    val l1: Long | String = identity(42L)

  // Nested inline matches
  inline def nestedDispatch(inline v: Long | String): String =
    inline v match
      case str: String =>
        inline str.length match
          case 0 => "empty"
          case _ => s"non-empty: $str"
      case long: Long => s"long: $long"

  def testNestedDispatch(): Unit =
    val r1 = nestedDispatch("")
    val r2 = nestedDispatch("hello")
    val r3 = nestedDispatch(s"interpolated")
    val r4 = nestedDispatch(42L)

  def main(args: Array[String]): Unit =
    testRegularStrings()
    testStringInterpolation()
    testFInterpolation()
    testRawInterpolation()
    testComplexDispatch()
    testIdentity()
    testNestedDispatch()
