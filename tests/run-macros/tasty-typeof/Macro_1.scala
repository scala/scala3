import scala.quoted._

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl(using Quotes) : Expr[Unit] = {
    import quotes.reflect._
    '{
      assert(${Value(TypeRepr.of[Unit] =:= TypeRepr.of[Unit])}, "Unit")
      assert(${Value(TypeRepr.of[Byte] =:= TypeRepr.of[Byte])}, "Byte")
      assert(${Value(TypeRepr.of[Short] =:= TypeRepr.of[Short])}, "Short")
      assert(${Value(TypeRepr.of[Int] =:= TypeRepr.of[Int])}, "Int")
      assert(${Value(TypeRepr.of[Long] =:= TypeRepr.of[Long])}, "Long")
      assert(${Value(TypeRepr.of[Float] =:= TypeRepr.of[Float])}, "Float")
      assert(${Value(TypeRepr.of[Double] =:= TypeRepr.of[Double])}, "Double")
      assert(${Value(TypeRepr.of[Char] =:= TypeRepr.of[Char])}, "Char")
      assert(${Value(TypeRepr.of[String] =:= TypeRepr.of[String])}, "String")

      assert(${Value(TypeRepr.of[Any] =:= TypeRepr.of[Any])}, "Any")
      assert(${Value(TypeRepr.of[AnyRef] =:= TypeRepr.of[AnyRef])}, "AnyRef")
      assert(${Value(TypeRepr.of[AnyVal] =:= TypeRepr.of[AnyVal])}, "AnyVal")
      assert(${Value(TypeRepr.of[Object] =:= TypeRepr.of[Object])}, "Object")
      assert(${Value(TypeRepr.of[Nothing] =:= TypeRepr.of[Nothing])}, "Nothing")

      println(${Value(TypeRepr.of[List[Int]].show)})
      println(${Value(TypeRepr.of[Macros].show)})
      println(${Value(TypeRepr.of[Macros.type].show)})
    }
  }

}

class Macros
