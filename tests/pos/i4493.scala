class Index[K]
object Index {
  transparent def succ[K]: Unit = ~{
    implicit val t: quoted.Type[K] = '[K]
    '(new Index[K])
  }
}
