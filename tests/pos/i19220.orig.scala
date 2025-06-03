object Test:
  class Custom extends scala.Product1[String]:
    def length: Int = ???
    def apply(i: Int): Boolean = ???
    def drop(n: Int): scala.Seq[Boolean] = ???
    def toSeq: scala.Seq[Boolean] = ???

    def canEqual(that: Any): Boolean = ???

    val _1: String = ???
    val _3: Seq[String] = ???

  class Unapplied:
    def isEmpty: Boolean = ???
    def get: Custom = ???

  object A:
    def unapplySeq(i: Int): Unapplied = ???

  val A(a, rest*) = 1
