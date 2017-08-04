trait Map2[K] {
  def get(k: K): K = k
  def foo: K = {
    this match {
      case that: Map2[c] => that.get(4.asInstanceOf[K])
    }
  }
}
