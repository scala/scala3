import scala.quoted._

abstract class Test {
  type T

  val T: Type[T]
  val getT: Type[T] = T // need this to avoid getting `null`
  given getT.type = getT

  def foo(using QuoteContext): Expr[Any] = {

    val r = '{Option.empty[T]}

    {
      val t: Test = this
      import t.given
      println(summon[Type[t.T]].show)
      // val r = '{Option.empty[t.T]} // access to value t from wrong staging level
      val r2 = '{Option.empty[${t.T}]}
    }

    {
      val r1 = '{Option.empty[${T}]} // works
      val r2 = '{Option.empty[List[${T}]]} // works
      // val r3 = '{summon[Type[${T}]]} // access to Test.this from wrong staging level
      val r4 = '{summon[${T} <:< Any]}
    }

    {
      val s = '{Option.empty[${T}]}
      val r = '{identity($s)} // works
      val r2 = '{identity(${s: Expr[Option[T]]})}
    }

    r
  }
}

@main def main = ()
