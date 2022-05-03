trait Foo extends reflect.Selectable

val foo = new Foo:
  def bar[A](): Int = ???

val x = foo.bar()      // error: Structural access not allowed on method bar because it is polymorphic

val y = foo.bar[Int]() // error: Structural access not allowed on method bar because it is polymorphic
