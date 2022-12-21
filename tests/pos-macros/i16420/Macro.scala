import scala.quoted.{Expr, Quotes, Type}

object Converter {
  private def handleUnit[R](f: Expr[Int ?=> R])(using q: Quotes, rt: Type[R]): Expr[Unit] = '{}

  class UnitConverter[R] extends Converter[EmptyTuple, R, Int ?=> R] {
    inline def convert(inline f: Int ?=> R): Unit = ${ handleUnit[R]('f) }
  }

  inline given unitHandler[R]: UnitConverter[R] = new UnitConverter[R]
}


trait Converter[T <: Tuple, R, F] {
  inline def convert(inline fn: F): Unit
}

abstract class Directive[R <: Tuple] {
  inline def apply[O, F](using inline c: Converter[R, O, F])(inline fn: F): Unit =
    c.convert(fn)
}
