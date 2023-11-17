// macros_0.scala
object source {
  import scala.quoted._

  class Position()

  object Position {
    def withPosition[T](
        fun: Expr[Position => T]
    )(using quotes: Quotes, typeOfT: Type[T]): Expr[T] = {
      '{
        ${ fun }.apply(new source.Position())
      }
    }
  }
}

trait AnyFreeSpecLike {
  import scala.language.implicitConversions

  protected final class FreeSpecStringWrapper(
      string: String,
      pos: source.Position
  ) {
    def -(fun: => Unit): Unit = fun
  }

  inline implicit def convertToFreeSpecStringWrapper(
      s: String
  ): FreeSpecStringWrapper = {
    ${
      source.Position.withPosition[FreeSpecStringWrapper]('{
        (pos: source.Position) => new FreeSpecStringWrapper(s, pos)
      })
    }
  }
}
