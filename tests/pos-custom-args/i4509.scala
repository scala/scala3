object Main {
  def fun[T](op: (erased Int) ?=> T) = op(using 0)
  fun { }
}
