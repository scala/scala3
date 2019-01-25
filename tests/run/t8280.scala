import scala.language.implicitConversions

object Test {
  def main(args: Array[String]): Unit = {
    Moop1.ob1
    Moop1.ob2
    Moop1.ob3
    Moop2.ob1
    Moop2.ob2
    Moop2.ob3
    Moop3.ob1
    Moop3.ob2
    Moop3.ob3
  }
}

// int object vs.
object Moop1 {
  object ob1 {
    implicit object f1 extends (Int => String) { def apply(x: Int): String = "Int" }
    implicit object f2 extends (Long => String) { def apply(x: Long): String = "Long" }

    // println(5: String)
      // Dotty deviation. The above fails for Dotty with ambiguity error.
      // Both f1 and f2 are applicable conversions for Int => String. Neither is better than the other.
      // Scala2 contains a hack for backwards compatibility, which we are not forced to repeat.
      // See discussion under SI-8280.

  }
  object ob2 {
    implicit object f1 extends (Int => String) { def apply(x: Int): String = "Int" }
    implicit def f2(x: Long): String = "Long"

    println(5: String)
  }
  object ob3 {
    implicit object f1 extends (Int => String) { def apply(x: Int): String = "Int" }
    implicit val f2: Long => String = _ => "Long"

    //println(5: String)
      // This picked f1 before, but is now disallowed since subtypes of functions are no longer implicit conversions
  }
}

// int def vs.
object Moop2 {
  object ob1 {
    implicit def f1(x: Int): String = "Int"
    implicit object f2 extends (Long => String) { def apply(x: Long): String = "Long" }

    println(5: String) // Dotty deviation: Dotty picks f2, because non-methods are more specific than methods
  }
  object ob2 {
    implicit def f1(x: Int): String = "Int"
    implicit def f2(x: Long): String = "Long"

    println(5: String)
  }
  object ob3 {
    implicit def f1(x: Int): String = "Int"
    implicit val f2: Long => String = _ => "Long"

    println(5: String)
  }
}

// int val vs.
object Moop3 {
  object ob1 {
    implicit val f1: Int => String  = _ => "Int"
    implicit object f2 extends (Long => String) { def apply(x: Long): String = "Long" }

    // println(5: String)
      // Dotty deviation. This fails for Dotty with ambiguity error for similar reasons as ob1.
  }
  object ob2 {
    implicit val f1: Conversion[Int, String]  = _ => "Int"
    implicit def f2(x: Long): String = "Long"

    println(5: String)
  }
  object ob3 {
    implicit val f1: Conversion[Int, String]  = _ => "Int"
    implicit val f2: Conversion[Long, String] = _ => "Long"

    println((5: Int): String)
    // println(5: String)  // error: ambiguity, since both f1 and f2 are applicable to 5.
  }
}

