object EmptyCaseClassParams{
  case class A[T] // error

  class C
  case class B[T] // error
    extends C

  enum Foo[T]{
    case D[T] extends Foo[T] // error
  }
}
