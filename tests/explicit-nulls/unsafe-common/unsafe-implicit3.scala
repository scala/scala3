class S {
  import scala.language.implicitConversions

  class C[T](x: T) {}

  locally {
    given Conversion[String, Array[String]] = _ => ???

    val y1: String = ???
    val y2: String | Null = ???

    y1: Array[String]
    y1: Array[String | Null] // error
    y1: Array[String] | Null
    y1: Array[String | Null] | Null // error

    y2: Array[String] // error
    y2: Array[String | Null] // error
    y2: Array[String] | Null // error
    y2: Array[String | Null] | Null // error
  }

  locally {
    given Conversion[Array[String], String] = _ => ???

    val y1: Array[String] = ???
    val y2: Array[String] | Null = ???
    val y3: Array[String | Null] | Null = ???

    y1: String
    y2: String // error
    y3: String // error

    y1: String | Null
    y2: String | Null // error
    y3: String | Null // error
  }

  locally {
    given Conversion[Array[String | Null], String] = _ => ???

    val y1: Array[String] = ???
    val y2: Array[String] | Null = ???
    val y3: Array[String | Null] = ???
    val y4: Array[String | Null] | Null = ???

    y1: String // error
    y2: String // error
    y3: String
    y4: String // error

    y1: String | Null // error
    y2: String | Null // error
    y3: String | Null
    y4: String | Null // error
  }

  locally {
    given Conversion[C[Array[String | Null]], String] = _ => ???

    val y1: C[Array[String]] = ???
    val y2: C[Array[String] | Null] = ???
    val y3: C[Array[String | Null]] = ???
    val y4: C[Array[String | Null] | Null] = ???
    val y5: C[Array[String | Null] | Null] | Null = ???

    y1: String // error
    y2: String // error
    y3: String
    y4: String // error
    y5: String // error

    y1: String | Null // error
    y2: String | Null // error
    y3: String | Null
    y4: String | Null // error
    y5: String | Null // error
  }

  abstract class MyConversion[T] extends Conversion[T, Array[T]]

  locally {
    given MyConversion[String] = _ => ???

    val y1: String = ???
    val y2: String | Null = ???

    val z1: Array[String] = y1
    val z2: Array[String | Null] = y1 // error
    val z3: Array[String] | Null = y1
    val z4: Array[String | Null] | Null = y1 // error

    val z5: Array[String] = y2 // error
    val z6: Array[String | Null] = y2 // error
    val z7: Array[String] | Null = y2 // error
    val z8: Array[String | Null] | Null = y2 // error
  }

  def test5[T >: Null <: AnyRef | Null] = {
    given Conversion[T, Array[T]] = _ => ???

    val y1: T = ???
    val y2: T | Null = ???

    val z1: Array[T] = y1
    val z2: Array[T | Null] = y1
    val z3: Array[T] | Null = y1
    val z4: Array[T | Null] | Null = y1

    val z5: Array[T] = y2
    val z6: Array[T | Null] = y2
    val z7: Array[T] | Null = y2
    val z8: Array[T | Null] | Null = y2
  }
}