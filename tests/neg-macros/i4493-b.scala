import scala.quoted._
class Index[K]
object Index {
  inline def succ[K](x: K): Unit = ${
    implicit val t: Type[K] = Type.of[K] // error
    '{new Index[K]} // error
  }
}
