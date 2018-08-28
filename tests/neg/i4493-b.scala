class Index[K]
object Index {
  rewrite def succ[K](x: K): Unit = ~{ // error
    implicit val t: quoted.Type[K] = '[K]
    '(new Index[K])
  }
}
