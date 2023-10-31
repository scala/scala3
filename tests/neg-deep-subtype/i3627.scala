trait Comparinator[T] {
  def sort[T](x: Comparinator[? >: T]) = ()
  sort((a: Int) => true) // error
}

trait Comparinator2[T >: U, U] {
  def sort[TT](x: Comparinator2[? >: TT, U]) = ()
  sort((a: Int) => true) // error
}
