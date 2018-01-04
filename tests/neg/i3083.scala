    object Main {

      trait Literal {
        type F[T]
        def num(i: Int): F[Int]
      }

      trait Addition { self: Literal =>
        def add(l: F[Int], r: F[Int]): F[Int]
      }

      def expression(adder: Addition) = {
        import adder._
        add(num(1), num(2)) // error // error
        }
      }

      object Minimized {
        trait Literal {
          type F[T]
        }

        trait Addition { self: Literal =>
          def foo: F[Int]
        }

        object Main {
          def expression(adder: Addition) = { // error (?)
            adder.foo
          }
        }
      }
