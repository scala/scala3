trait X { self =>
  type R <: Z
  type Z >:
    X { // error
      type R = // was-error
        self.R
      type Z = // was-error
        self.R
    }
}

class Foo // error
    extends X {
  type R =
    Foo
  type Z =
    Foo
}
