class S {
  def m0(s: String): String = s

  def m1(s: String): String | Null = s

  def n0(x: Array[String]): Array[String] = x

  def n1(x: Array[String | Null]): Array[String | Null] = x

  def n2(x: Array[String | Null] | Null): Array[String | Null] | Null = x

  def test1 = {
    val s: String = ???

    val a1: String | Null = s // safe
    val a2: String = a1 // error

    val b1 = s.trim() // String | Null
    val b2 = b1.trim() // error
    val b3 = b1.length() // error

    val c1: String | Null = null // safe
    val c2: String = null // error
    val c3: Int | String = null // error

    val d1: Array[String | Null] | Null = Array(s)
    val d2: Array[String] = d1 // error
    val d3: Array[String | Null] = d2 // error
    val d4: Array[String] = Array(null) // error
  }

  def test2 = {
    m0("")
    m0(null) // error

    val a: String | Null = ???
    val b: String = m0(a) // error
    val c: String = m1(a).trim() // error

    val x: Array[String | Null] | Null = ???
    val y: Array[String] = ???
    val z: Array[String | Null] = ???

    n0(x) // error
    n0(y)
    n0(z) // error

    n1(x) // error
    n1(y) // error
    n1(z)

    n2(x)
    n2(y) // error
    n2(z)

    n0(Array("a", "b"))
    n1(Array("a", "b"))
    n2(Array("a", "b"))

    n0(Array[String](null)) // error
    n1(Array(null))
    n2(Array(null))

    n0(Array("a", null)) // error
    n1(Array("a", null))
    n2(Array("a", null))
  }

  locally {
    val os: Option[String] = None
    val s: String = os.orNull // error
  }
}