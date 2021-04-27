import scala.quoted.*
class Index[K]
object Index {
  inline def succ[K](x: K): Unit = ${
    implicit val t: Type[K] = Type.of[K] // error
    '{new Index[K]}
  }
}
