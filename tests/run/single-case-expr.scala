import language.experimental.relaxedLambdaSyntax

case class Foo(x: Int, y: Int)
@main def Test =
  val f: List[Int] => Int = case y :: ys => y
  val xs = List(1, 2, 3)
  assert(f(xs) == 1)

  val g: Foo => Int = identity(case Foo(a, b) => a)
  val foo = Foo(1, 2)
  assert(g(foo) == 1)

  val h1: Foo => Int = identity: case Foo(a, b) => a
  val h2: Foo => Int = identity: case Foo(a, b) =>
    a

  val a1 = Seq((1, 2), (3, 4)).collect(case (a, b) if b > 2 => a)
  assert(a1 == Seq(3))

  var a2 = Seq((1, 2), (3, 4)).collect(
    case (a, b) =>
      println(b)
      a
  )
  assert(a2 == Seq(1, 3))

  val a3 = Seq((1, 2), (3, 4)).collect: case (a, b) if b > 2 => a
  assert(a3 == Seq(3))

  val a4 = Seq((1, 2), (3, 4)).collect: case (a, b) if b > 2 =>
    a

  val partial: PartialFunction[(Int, Int), Int] = case (a, b) if b > 2 => a

  val mtup = (1, true).map: [T] => (x: T) => List(x)
  val _: (List[Int], List[Boolean]) = mtup



