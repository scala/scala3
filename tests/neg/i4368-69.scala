object i4369 {
  trait X { self =>
    type R <: Z
    type Z >: X { type R = self.R; type Z = self.R } // error: cyclic
  }
  class Foo extends X { type R = Foo; type Z = Foo } // error
}
