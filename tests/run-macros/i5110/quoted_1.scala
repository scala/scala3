import scala.quoted.*

object Macro {

  class Boom extends Exception

  class Bomb {
    throw new Boom
  }

  extension (boom: Bomb) inline def foo(): Unit = ()

  // By name Boom is used to elide the evaluation of the prefix
  extension (boom: => Bomb) inline def bar(): Unit = ()

}
