object trace {
  def apply[T](a: String, showOp: Any => String) = 0
  def apply[T](a: Int, log: String => Unit) = 0

  apply("", res => res.toString)
}
