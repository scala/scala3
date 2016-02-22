trait A { trait Inner }
trait B { self: A =>
  trait Inner extends self.Inner // error: cannot merge trait Inner in trait A with trait Inner in trait B as members of type (A & B)(B.this)
}


class C extends C // error: cyclic inheritance: class C extends itself
