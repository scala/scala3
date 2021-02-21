// Test that reference types are no longer nullable.

class Basic {
  val no: Nothing = ???

  val n: Null = null
  val n2: Null = no

  val any1: Any  = null
  val any2: Any  = n

  val s1: String = null // error
  val s2: String = n // error
  val s3: String | Null = null
  val s4: String | Null = n
  val s5: String | Null = ""

  val ar1: AnyRef = null // error
  val ar2: AnyRef = n // error
  val ar3: AnyRef | Null = null
  val ob1: Object = null // error
  val ob2: Object | Null = null

  val b1: Boolean = null // error
  val b2: Boolean = n // error
  val b3: Boolean | Null = null

  val c1: Int = null // error
  val c2: Int = n // error
  val i3: Int | Null = null

  val av: AnyVal = null // error
}
