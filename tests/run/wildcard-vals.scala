object O:
  val _ = 1
  val _ = 2
  val _: Int = 3
  val _: Int = 4
  def x = 2

@main def Test =
  assert(O.x == 2)
  val _ = 1
  val _ = 2
  val _: Int = 3
  val _: Int = 4

