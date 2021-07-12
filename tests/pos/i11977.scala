object Test:

  def add0(a : (Int, String), b : (Int, String)): Int =
    val x = 3
    x + b(0)

  def add(a : (Int, String), b : (Int, String)) : (Int, String) = (a(0) + b(0), a(1) + b(1))
