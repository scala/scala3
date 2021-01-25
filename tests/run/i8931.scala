object test1:

  trait Trait

  trait Managed[T](x: T) {

    def flatMap(f: T => Managed[T]): Managed[T] = new Managed[T](x) {

      def make() = new Trait {
        val t: T = x
        val u =
          try {
            f(t)
          } catch {
            case e: Exception => ()
          }
      }
    }
  }

object test2:

  trait Trait

  trait Managed[T](x: T) {
    def xx = x

    def flatMap(f: T => Managed[T]): Managed[T] = new Managed[T](x) {
      def make() = new Trait {
        val t: T = x
        val u = {
          def foo = f(t)
          assert(foo.xx == 22)
          foo
        }
      }
      make()
    }
  }

object test3:

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

@main def Test() =
  val m = new test2.Managed[Int](22) {}
  m.flatMap(x => new test2.Managed(x) {})
