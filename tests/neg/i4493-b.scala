class Index[K]
object Index {
  inline def succ[K](x: K): Unit = ${
    implicit val t: quoted.Type[K] = '[K] // error
    '{new Index[K]}
  }
}
