trait T { self: {type M = Int} =>
    type T = self.M
    def lift(x: Int): T = x
  }

  class Test {
    val t = new T {
      type M = Int
    }
    t.lift(1): t.T // error
  }