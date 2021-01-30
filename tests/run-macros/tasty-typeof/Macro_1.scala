import scala.quoted.*

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    '{
      assert(${Expr(TypeRepr.of[Unit] =:= TypeRepr.of[Unit])}, "Unit")
      assert(${Expr(TypeRepr.of[Byte] =:= TypeRepr.of[Byte])}, "Byte")
      assert(${Expr(TypeRepr.of[Short] =:= TypeRepr.of[Short])}, "Short")
      assert(${Expr(TypeRepr.of[Int] =:= TypeRepr.of[Int])}, "Int")
      assert(${Expr(TypeRepr.of[Long] =:= TypeRepr.of[Long])}, "Long")
      assert(${Expr(TypeRepr.of[Float] =:= TypeRepr.of[Float])}, "Float")
      assert(${Expr(TypeRepr.of[Double] =:= TypeRepr.of[Double])}, "Double")
      assert(${Expr(TypeRepr.of[Char] =:= TypeRepr.of[Char])}, "Char")
      assert(${Expr(TypeRepr.of[String] =:= TypeRepr.of[String])}, "String")

      assert(${Expr(TypeRepr.of[Any] =:= TypeRepr.of[Any])}, "Any")
      assert(${Expr(TypeRepr.of[AnyRef] =:= TypeRepr.of[AnyRef])}, "AnyRef")
      assert(${Expr(TypeRepr.of[AnyVal] =:= TypeRepr.of[AnyVal])}, "AnyVal")
      assert(${Expr(TypeRepr.of[Object] =:= TypeRepr.of[Object])}, "Object")
      assert(${Expr(TypeRepr.of[Nothing] =:= TypeRepr.of[Nothing])}, "Nothing")

      println(${Expr(TypeRepr.of[List[Int]].show)})
      println(${Expr(TypeRepr.of[Macros].show)})
      println(${Expr(TypeRepr.of[Macros.type].show)})
    }
  }

}

class Macros
