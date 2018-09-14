class TypeAlias {
  type T[X] = X
  def a(i: T[Int]): T[Int] = i
}
