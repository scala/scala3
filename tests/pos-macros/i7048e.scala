import scala.quoted.*

abstract class Test {
  type T

  val T: Type[T]
  val getT: Type[T] = T // need this to avoid getting `null`
  given getT.type = getT

  def foo(using Quotes): Expr[Any] = {

    val r = '{Option.empty[T]}

    {
      val t: Test = this
      import t.given
      println(Type.show[t.T])
      // val r = '{Option.empty[t.T]} // access to value t from wrong staging level
      val r2 = '{Option.empty[t.T.Underlying]}
    }

    {
      val r1 = '{Option.empty[T.Underlying]} // works
      val r2 = '{Option.empty[List[T.Underlying]]} // works
      // val r3 = '{summon[Type[T.Underlying]]} // access to Test.this from wrong staging level
      val r4 = '{summon[T.Underlying <:< Any]}
    }

    {
      val s = '{Option.empty[T.Underlying]}
      val r = '{identity($s)} // works
      val r2 = '{identity(${s: Expr[Option[T]]})}
    }

    r
  }
}

@main def main = ()
