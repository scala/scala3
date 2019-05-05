object Main {
  def fun[T](op: given erased (Int) => T) = op given 0
  fun { }
}
