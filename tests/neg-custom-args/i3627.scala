trait Comparinator[T] {
  def sort[T](x: Comparinator[_ >: T]) = ()
  sort((a: Int) => true) // error
}

trait Comparinator2[T >: U, U] {
  def sort[TT](x: Comparinator2[_ >: TT, U]) = ()
  sort((a: Int) => true) // error
}
