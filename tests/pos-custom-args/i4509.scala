object Main {
  def fun[T](op: (erased Int) ?=> T) = op(given 0)
  fun { }
}
