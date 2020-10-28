class Index[K]
object Index {
  inline def succ[K](x: K): Unit = ${
    implicit val t: quoted.Type[K] = Type[K] // error
    '{new Index[K]}
  }
}
