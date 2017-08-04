package strawman.collection

trait Map2[K] {
  def get(k: K): K = k
  def foo: K = {
    this match {
      case that: Map2[b] => that.get(3.asInstanceOf[b])
      // case that: Map2[b] => that.get(3.asInstanceOf[K]) // FIXME
      case _ => get(5.asInstanceOf[K])
    }
  }
}
