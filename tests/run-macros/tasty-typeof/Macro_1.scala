import scala.quoted._

object Macros {

  inline def testTypeOf(): Unit = ${ testTypeOfImpl }

  private def testTypeOfImpl(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    '{
      assert(${Expr(Type.of[Unit] =:= Type.of[Unit])}, "Unit")
      assert(${Expr(Type.of[Byte] =:= Type.of[Byte])}, "Byte")
      assert(${Expr(Type.of[Short] =:= Type.of[Short])}, "Short")
      assert(${Expr(Type.of[Int] =:= Type.of[Int])}, "Int")
      assert(${Expr(Type.of[Long] =:= Type.of[Long])}, "Long")
      assert(${Expr(Type.of[Float] =:= Type.of[Float])}, "Float")
      assert(${Expr(Type.of[Double] =:= Type.of[Double])}, "Double")
      assert(${Expr(Type.of[Char] =:= Type.of[Char])}, "Char")
      assert(${Expr(Type.of[String] =:= Type.of[String])}, "String")

      assert(${Expr(Type.of[Any] =:= Type.of[Any])}, "Any")
      assert(${Expr(Type.of[AnyRef] =:= Type.of[AnyRef])}, "AnyRef")
      assert(${Expr(Type.of[AnyVal] =:= Type.of[AnyVal])}, "AnyVal")
      assert(${Expr(Type.of[Object] =:= Type.of[Object])}, "Object")
      assert(${Expr(Type.of[Nothing] =:= Type.of[Nothing])}, "Nothing")

      println(${Expr(Type.of[List[Int]].show)})
      println(${Expr(Type.of[Macros].show)})
      println(${Expr(Type.of[Macros.type].show)})
    }
  }

}

class Macros
