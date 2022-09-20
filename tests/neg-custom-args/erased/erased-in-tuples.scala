@main def Test() =
  val x = 5
  val y = 7

  val t1 = (x, erased y) // error
  val t2 = (erased x, y) // error
  val t1a = (x: Int, erased y: Int) // error
  val t2a = (erased x: Int, y: Int) // error

  val nest = (x, (x, erased y)) // error

  def use(f: (Int, Int) => Any) = f(5, 6)

  use((_, erased _)) // error

  (x, erased y) // error
