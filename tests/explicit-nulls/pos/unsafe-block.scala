def trim(x: String | Null): String =
  import scala.language.unsafeNulls
  // The type of `x.trim()` is `String | Null`.
  // Although `String | Null` conforms the expected type `String`,
  // we still need to cast the expression to the expected type here,
  // because outside the scope we don't have `unsafeNulls` anymore.
  x.trim()

class TestDefs:

  def f1: String | Null = null
  def f2: Array[String | Null] | Null = null
  def f3: Array[String] | Null = null

  def h1a: String =
    import scala.language.unsafeNulls
    f1

  def h1b: String | Null =
    import scala.language.unsafeNulls
    f1

  def h2a: Array[String] =
    import scala.language.unsafeNulls
    f2

  def h2b: Array[String | Null] =
    import scala.language.unsafeNulls
    f2

  def h3a: Array[String] =
    import scala.language.unsafeNulls
    f3

  def h3b: Array[String | Null] =
    import scala.language.unsafeNulls
    f3

class TestVals:

  val f1: String | Null = null
  val f2: Array[String | Null] | Null = null
  val f3: Array[String] | Null = null

  val h1a: String =
    import scala.language.unsafeNulls
    f1

  val h1b: String | Null =
    import scala.language.unsafeNulls
    f1

  val h2a: Array[String] =
    import scala.language.unsafeNulls
    f2

  val h2b: Array[String | Null] =
    import scala.language.unsafeNulls
    f2

  val h3a: Array[String] =
    import scala.language.unsafeNulls
    f3

  val h3b: Array[String | Null] =
    import scala.language.unsafeNulls
    f3