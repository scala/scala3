@main def test() =
  given Int = 42

  locally:
    given String = "y"
    def ff(implicit i: Int, s: String = "x") = s * i
    def gg(using i: Int, s: String = "x") = s * i
    ff(using i = 10) // warn uses default arg
    gg(using i = 10) // warn
    ff(i = 10) // warn should use using
    gg(i = 10) // remarkably, augmentString(gg(given_Int, given_String)).apply(i = 10)
