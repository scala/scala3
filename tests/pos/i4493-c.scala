class Index[K]
object Index {
  rewrite def succ[K]: Unit = ~{
    '(new Index[K])
  }
}
