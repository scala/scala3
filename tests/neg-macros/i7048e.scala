import scala.quoted.*

abstract class Test {
  type T

  val T: Type[T]
  val getT: Type[T] = T // need this to avoid getting `null`
  given Type[T] = getT

  def foo(using Quotes): Expr[Any] = {

    val r = '{Option.empty[T]} // works

    {
      val t: Test = this
      import t.given
      println(Type.show[t.T])
      val r = '{Option.empty[t.T]} // works
      val r2 = '{Option.empty[t.T.Underlying]} // works
    }

    {
      val r1 = '{Option.empty[T.Underlying]} // works
      val r2 = '{Option.empty[List[T.Underlying]]} // works
      val r3 = '{summon[Type[T.Underlying]]} // error: access to value t from wrong staging level
      val r4 = '{summon[T.Underlying <:< Any]} // works
    }

    {
      val s = '{Option.empty[T.Underlying]} // works
      val r = '{identity($s)} // works
      val r2 = '{identity(${s: Expr[Option[T]]})} //works
    }

    r
  }
}

@main def tester = ()
