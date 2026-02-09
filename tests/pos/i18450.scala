//> using options -explain-cyclic -Ydebug-cyclic

class C:
  extension (s: String)
    def f = "hello, world"
    def g = f

    //def k = k // Overloaded or recursive method k needs return type
    // if k is not forced, it fails with:
    // value k is not a member of String.
    // Extension methods were tried, ...

    def e =
      import this.{f as hw}
      hw // this.f
