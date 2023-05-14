object Boom {
  import scala.compiletime.*
  trait Fail[A <: Int, B <: Int]

  transparent inline given fail[X <: Int, Y <: Int]: Fail[X, Y] = {
     scala.compiletime.summonFrom {
       case t: Fail[X, y] if constValue[y] < constValue[Y] => ???
    }
  }

  val a: Int = 1
  given ev1: Fail[a.type, 2] = null

  summon[Fail[a.type, 3]]
}
