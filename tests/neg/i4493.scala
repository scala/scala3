class Index[K]
object Index {
  inline def succ[K]: Unit = ${
    implicit val t: scala.quoted.TypeTag[K] = '[K] // error
    '{new Index[K]}
  }
}
