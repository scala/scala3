class S {
  locally {
    implicit val x: String = ???

    val y1: String = summon
    val y2: String | Null = summon
  }

  def test1(implicit x: String) = {
    val y1: String = summon
    val y2: String | Null = summon
  }

  def test2(using String) = {
    val y1: String = summon
    val y2: String | Null = summon
  }

  locally {
    implicit val x: String | Null = ???

    val y1: String = summon // error
    val y2: String | Null = summon
  }

  def test3(implicit x: String | Null) = {
    val y1: String = summon // error
    val y2: String | Null = summon
  }

  def test4(using String | Null) = {
    val y1: String = summon // error
    val y2: String | Null = summon
  }

  import scala.language.implicitConversions

  locally {
    implicit def f(x: String): Array[String] = ???

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

  locally {
    given Conversion[String, Array[String]] = _ => ???

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

  def test5[T <: AnyRef] = {
    given Conversion[T, Array[T]] = _ => ???

    val y1: T = ???
    val y2: T | Null = ???

    val z1: Array[T] = y1
    val z2: Array[T | Null] = y1 // error
    val z3: Array[T] | Null = y1
    val z4: Array[T | Null] | Null = y1 // error

    val z5: Array[T] = y2 // error
    val z6: Array[T | Null] = y2 // error
    val z7: Array[T] | Null = y2 // error
    val z8: Array[T | Null] | Null = y2 // error
  }
}