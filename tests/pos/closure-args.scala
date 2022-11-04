import language.experimental.fewerBraces

object Test1:
  val xs = List(1, 2, 3)
  val ys = xs.map: x =>
    x + 1
  val ys1 = List(1) map: x =>
    x + 1
  val x = ys.foldLeft(0): (x, y) =>
    x + y
  val y = ys.foldLeft(0): (x: Int, y: Int) =>
    val z = x + y
    z * z
  val a: Int = xs
    .map: x =>
      x * x
    .filter: (y: Int) =>
      y > 0
    (0)
  val e = xs.map:
      case 1 => 2
      case 2 => 3
      case x => x
    .filter:
      x => x > 0

  extension (xs: List[Int]) def foo(f: [X] => X => X) = ()

  val p = xs.foo:
    [X] => (x: X) => x

  val q = (x: String => String) => x

  val r = x < 0 && locally:
    y > 0

