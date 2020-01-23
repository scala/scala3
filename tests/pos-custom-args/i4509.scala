object Main {
  def fun[T](op: (erased Int) ?=> T) = op.with(0)
  fun { }
}
