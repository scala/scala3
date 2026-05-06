//> using options --explain

object Test:
  locally:
    def f: Int = g // error
    var x: Int = f
    def g: Int = x
