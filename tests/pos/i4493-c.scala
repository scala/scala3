class Index[K]
object Index {
  transparent def succ[K]: Unit = ~{
    '(new Index[K])
  }
}
