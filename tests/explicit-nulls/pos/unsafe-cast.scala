class S {
  def m0(s: String): String = s

  def m1(s: String): String | Null = s

  def n0(x: Array[String]): Array[String] = x

  def n1(x: Array[String | Null]): Array[String | Null] = x

  def n2(x: Array[String | Null] | Null): Array[String | Null] | Null = x

  import scala.language.unsafeNulls

  def test1 = {
    val s: String = ???

    val a1: String | Null = s // safe
    val a2: String = a1 // String | Null to String

    val b1 = s.trim() // String | Null
    val b2 = b1.trim()
    val b3 = b2.length()

    val c1: String | Null = null // safe
    val c2: String = null // Null to String
    val c3: Int | String = null

    val d1: Array[String | Null] | Null = Array(s) // Array[String] to Array[String  | Null]
    val d2: Array[String] = d1
    val d3: Array[String | Null] = d2
    val d4: Array[String] = Array(null)
  }

  def test2 = {
    m0("")
    m0(null)

    val a: String | Null = ???
    val b: String = m0(a)
    val c: String = m1(a).trim()

    val x: Array[String | Null] | Null = ???
    val y: Array[String] = x
    val z: Array[String | Null] = y

    n0(x)
    n0(y)
    n0(z)

    n1(x)
    n1(y)
    n1(z)

    n2(x)
    n2(y)
    n2(z)

    n0(Array("a", "b"))
    n1(Array("a", "b"))
    n2(Array("a", "b"))

    n0(Array(null))
    n1(Array(null))
    n2(Array(null))

    n0(Array("a", null))
    n1(Array("a", null))
    n2(Array("a", null))
  }

  def test[T <: AnyRef](x: T | Null): T = {
    val y: T = x
    val z: T = null
    x
  }
}