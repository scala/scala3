import scala.quoted._

abstract class Test {
  type T

  val T: Type[T]
  val getT: Type[T] = T // need this to avoid getting `null`
  given Type[T] = getT

  def foo(using QuoteContext): Expr[Any] = {

    val r = '{Option.empty[T]} // error: is not stable

    {
      val t: Test = this
      import t.{given _}
      println(summon[Type[t.T]].show)
      // val r = '{Option.empty[t.T]} // access to value t from wrong staging level
      val r2 = '{Option.empty[t.T.T]} // works
    }

    {
      val r1 = '{Option.empty[T.T]} // works
      val r2 = '{Option.empty[List[T.T]]} // works
      val r3 = '{summon[Type[T.T]]} // error: is not stable
      val r4 = '{summon[T.T <:< Any]} // error: is not stable
    }

    {
      val s = '{Option.empty[T.T]} // works
      val r = '{identity($s)} // works
      val r2 = '{identity(${s: Expr[Option[T]]})} // error // error : is not stable
    }

    r
  }
}

@main def main = ()
