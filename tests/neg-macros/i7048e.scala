import scala.quoted._

abstract class Test {
  type T

  given s as Scope

  val T: s.Type[T]
  val getT: s.Type[T] = T // need this to avoid getting `null`
  given s.Type[T] = getT

  def foo: s.Expr[Any] = {

    val r = '{Option.empty[T]} // error: is not stable

    {
      val t: Test = this
      import t.{given _}
      println(summon[t.s.Type[t.T]].show)
      // val r = '{Option.empty[t.T]} // access to value t from wrong staging level
      val r2 = '{Option.empty[${t.T}]} // works
    }

    {
      val r1 = '{Option.empty[${T}]} // works
      val r2 = '{Option.empty[List[${T}]]} // works
      val r3 = '{summon[s.Type[${T}]]} // error: is not stable
      val r4 = '{summon[${T} <:< Any]} // error: is not stable
    }

    {
      val r0 = '{Option.empty[${T}]} // works
      val r1 = '{identity($r0)} // works
      val r2 = '{identity(${r0: s.Expr[Option[T]]})} // error // error : is not stable
    }

    r
  }
}

@main def main = ()
