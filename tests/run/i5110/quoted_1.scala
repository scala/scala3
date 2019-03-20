import scala.quoted._

object Macro {

  class Boom extends Exception

  class Bomb {
    throw new Boom
  }

  inline def (boom: Bomb) foo(): Unit = ()

  // By name Boom is used to elide the evaluation of the prefix
  inline def (boom: => Bomb) bar(): Unit = ()

}
