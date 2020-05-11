trait Trait

trait Managed[T]:

  def flatMap[U](f: T => Managed[U]) =
    class C:
      def make() =
        class D:
          def bar(): T = ???
          val t: T = ???
          val u =
            def foo = (f(t), f(bar()))
            foo
        new D().u
    ()
