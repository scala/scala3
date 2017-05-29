// To be kept in sync with docs/docs/reference/pattern-matching.md
object Test {
  def main(args: Array[String]): Unit = {
    object Even {
      def unapply(s: String): Boolean = s.size % 2 == 0
    }

    "even" match {
      case s @ Even() => println(s"$s has an even number of characters")
      case s          => println(s"$s has an odd number of characters")
    }
    // even has an even number of characters

    class FirstChars(s: String) extends Product {
      def _1 = s.charAt(0)
      def _2 = s.charAt(1)

      // Not used by pattern matching: Product is only used as a marker trait.
      def canEqual(that: Any): Boolean = ???
      def productArity: Int = ???
      def productElement(n: Int): Any = ???
    }

    object FirstChars {
      def unapply(s: String): FirstChars = new FirstChars(s)
    }

    "Hi!" match {
      case FirstChars(char1, char2) =>
        println(s"First: $char1; Second: $char2")
    }
    // First: H; Second: i

    object CharList {
      def unapplySeq(s: String): Option[Seq[Char]] = Some(s.toList)
    }

    "example" match {
      case CharList(c1, c2, c3, c4, _, _, _) =>
        println(s"$c1,$c2,$c3,$c4")
      case _ =>
        println("Expected *exactly* 7 characters!")
    }
    // e,x,a,m

    class Nat(val x: Int) {
      def get: Int = x
      def isEmpty = x < 0
    }

    object Nat {
      def unapply(x: Int): Nat = new Nat(x)
    }

    5 match {
      case Nat(n) => println(s"$n is a natural number")
      case _      => ()
    }
    // 5 is a natural number
  }
}
