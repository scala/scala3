class A {
  type Foo[T] = T match {
    case Int => String
    case T => T
  }

  // FIXME: These cases are not disallowed currently and cause
  // infinite loops
  /*
  def a1[T <: (T match {
    case Int => String
    case T => T
  })]: T = ???
  a1

  def a2[T, U >: T <: (T match {
    case Int => String
    case T => T
  })]: T = ???
  a2

  def a3[T <: Foo[T]]: T = ???
  a3
  */

  def a4[T, U >: T <: Foo[T]]: T = ???
  a4 // error
}
