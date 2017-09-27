class Test[A](action: A => A) {
  def this() = this(a => a)
}