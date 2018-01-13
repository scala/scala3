object Minimized {
  trait Literal {
    type F[T]
  }

  trait Addition { self: Literal =>
    def foo: F[Int]
  }

  object Main {
    def expression(adder: Addition & Literal) = { // error: adder.F is not defined in inferred type
      adder.foo
    }
  }
}
