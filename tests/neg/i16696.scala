object test1:
  class BoxMaker[T] {
    def make1: T & Int = ???
    def make2: T | Int = ???
  }

  val boom1 = BoxMaker[Some].make1 // error
  val boom2 = BoxMaker[Some].make2 // error

object test2:
  class Box[R]

  class BoxMaker[T] {
    def make[R <: T](f: T => Box[R]): Box[R & T] = ???
  }

  trait Foo[A]{
    def foo: Box[Foo[Unit]]
  }
  val boom = BoxMaker[Foo].make(_.foo) // error
