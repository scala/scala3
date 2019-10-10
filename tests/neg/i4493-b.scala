class Index[K]
object Index {
  inline def succ[K](x: K): Unit = ${
    implicit val t: quoted.TypeTag[K] = '[K] // error
    '{new Index[K]}
  }
}
