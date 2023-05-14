object Test:

  def transparent = println("transparent method called")

  transparent
  println()
  inline def f1 = 1

  transparent
  inline def f2 = 2

  transparent
  trait T1

  transparent

  inline def f3 = 3

  transparent

  trait T2