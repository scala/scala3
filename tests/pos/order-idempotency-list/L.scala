trait L[+T] {
  def head: T
  def tail: L[T]
}
