val xs = List(1, 2, 3)
val ys = xs.map x =>
  x + 1
val x = ys.foldLeft(0) (x, y) =>
  x + y
val y = ys.foldLeft(0) (x: Int, y: Int) =>
  val z = x + y
  z * z
val as: Int = xs
  .map x =>
    x * x
  .filter y =>
    y > 0
  (0)
