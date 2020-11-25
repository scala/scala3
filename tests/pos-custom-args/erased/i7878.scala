object Boom {
  import scala.compiletime._
  trait Fail[A <: Int, B <: Int]

  erased inline given fail[X <: Int, Y <: Int] as Fail[X, Y] = {
     scala.compiletime.summonFrom {
       case t: Fail[X, y] if constValue[y] < constValue[Y] => ???
    }
  }

  val a: Int = 1
  given Fail[a.type, 2] as ev1 = null

  summon[Fail[a.type, 3]]
}