object preciseLib:
  import scala.annotation.precise

  object internals:
    trait Foo[V]
    def tester[@precise V](v: V): Foo[V] = ???
    extension [T](t: T) def testerExt[@precise V](v: V): Foo[V] = ???

  export internals.*