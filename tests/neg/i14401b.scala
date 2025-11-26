//> using options --explain

object Test:
  locally:
    def f: Int = x; // error
    val x: Int = f;
