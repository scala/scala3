object Test:

  def f(erased x: Int) =
    println("f")
    42

  val x = f // error

