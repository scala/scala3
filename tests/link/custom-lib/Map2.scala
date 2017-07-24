trait Map2[K] {
  def get(k: K): K = k
  def foo: K = {
    this match {
      case that: Map2[b] => that.get(3.asInstanceOf[b])
      case _ => get(5.asInstanceOf[K])
    }
  }
}
