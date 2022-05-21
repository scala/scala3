import language.experimental.fewerBraces

object Test1:
  val xs = List(1, 2, 3)
  val ys = xs.map: x =>
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
  val b: Int = xs
    .map: x => x * x
    .filter: y => y > 0
    (0)
  val c = List(xs.map: y => y + y)
  val d: String = xs
    .map: x => x.toString + xs.dropWhile: y => y > 0
    .filter: z => !z.isEmpty
    (0)
  val e = xs.map:
      case 1 => 2
      case 2 => 3
      case x => x
    .filter: x => x > 0
  val fs: List[List[Int] => Int] = xs.map: x => case y :: ys => y case Nil => -1

  extension (xs: List[Int]) def foo(f: [X] => X => X) = ()

  val p = xs.foo: [X] => (x: X) => x

  val q = (x: String => String) => x
