trait A { trait Inner }
trait B { self: A =>
  trait Inner extends self.Inner
}


class C extends C
