package example

/**
  * Pattern Matching: https://nightly.scala-lang.org/docs/reference/changed-features/pattern-matching.html
  */
object PatternMatching {

  object booleanPattern {

    object Even {
      def unapply(s: String): Boolean = s.length % 2 == 0
    }

  }

  object productPattern {

    class Person(name: String, age: Int) extends Product {
      // if we not define that, it will give compile error.
      // we change the order
      def _1 = age
      def _2 = name

      // Not used by pattern matching: Product is only used as a marker trait.
      def canEqual(that: Any): Boolean = ???
      def productArity: Int = ???
      def productElement(n: Int): Any = ???
    }

    object Person {
      def unapply(a: (String, Int)): Person = new Person(a._1, a._2)
    }

  }

  object seqPattern {

    // adapted from http://danielwestheide.com/blog/2012/11/28/the-neophytes-guide-to-scala-part-2-extracting-sequences.html
    object Names {
      def unapplySeq(name: String): Option[Seq[String]] = {
        val names = name.trim.split(" ")
        if (names.size < 2) None
        else Some(names.last :: names.head :: names.drop(1).dropRight(1).toList)
      }
    }

  }

  object namePattern {

    class Name(val name: String) {
      def get: String = name
      def isEmpty = name.isEmpty
    }

    object Name {
      def unapply(s: String): Name = new Name(s)
    }

  }

  def test: Unit = {

    import booleanPattern._

    "even" match {
      case s @ Even() => println(s"$s has an even number of characters")
      case s => println(s"$s has an odd number of characters")
    }

    // https://nightly.scala-lang.org/docs/reference/changed/vararg-patterns.html
    def containsConsecutive(list: List[Int]): Boolean = list match {
      case List(a, b, xs: _ *) => if (a == b) true else containsConsecutive(b :: xs.toList)
      case List(a, _ : _*) => false
      case Nil => false
    }

    println(containsConsecutive(List(1, 2, 3, 4, 5)))
    println(containsConsecutive(List(1, 2, 3, 3, 5)))

    import productPattern._
    ("john", 42) match {
      case Person(n, a) => println(s"name: $n, age: $a")
    }

    import seqPattern._

    def greet(fullName: String) = fullName match {
      case Names(lastName, firstName, _: _*) => "Good morning, " + firstName + " " + lastName + "!"
      case _ => "Welcome! Please make sure to fill in your name!"
    }

    println(greet("Alan Turing"))
    println(greet("john"))
    println(greet("Wolfgang Amadeus Mozart"))

    import namePattern._
    "alice" match {
      case Name(n) => println(s"name is $n")
      case _ => println("empty name")
    }

  }
}
