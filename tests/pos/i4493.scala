class Index[K]
object Index {
  inline def succ[K]: Unit = ~{
    implicit val t: quoted.Type[K] = '[K]
    '(new Index[K])
  }
}
