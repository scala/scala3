trait A:

  type Ctx[T]
  type Mega[T]

  def f: Ctx[Ctx[Int]]
  def g(x: Boolean): Ctx[Int]
  val a: Ctx[Int]
  def b: Ctx[Int]

  def h(x: Int): String ?=> Int
  def mega: Mega[Int]

  def trans(x: Ctx[Int]): Ctx[Int] = x
end A

object m extends A:

  type Ctx[T] = String ?=> T
  type Mega[T] = (Int, Int, Int, Int, Int,
                  Int, Int, Int, Int, Int,
                  Int, Int, Int, Int, Int,
                  Int, Int, Int, Int, Int,
                  Int, Int, String) ?=> T

  def f: Ctx[Ctx[Int]] = summon[String].length

  def g(x: Boolean): Ctx[Int] =
    if x then summon[String].length else 0
  val a: Ctx[Int] = summon[String].length
  var b: Ctx[Int] = compiletime.uninitialized
  b = summon[String].length

  def h(x: Int): Ctx[Int] = x + g(true) + f + a + b

  def mega: Mega[Int] = summon[String].length
end m

trait B:

  type Ctx[T]

  def wrap[T](x: T): Ctx[T]
  def drop[T](x: Ctx[T]): T
  def id[T](x: T): T = drop(wrap(x))

end B
object n extends B:
  type Ctx[T] = String ?=> Int ?=> T

  def wrap[T](x: T): Ctx[T] = x
  def drop[T](x: Ctx[T]): T = x(using "X")(using 22)
end n

@main def Test =
  assert(m.h(1)(using "abc") == 13)
  val a: A = m
  assert(a.h(1)(using "abc") == 13)
  locally {
    given Int = 2
    given String = "abc"
    assert(m.mega == 3)
    assert(m.trans(m.a) == 3)
  }
  assert(n.id(1.0) == 1.0)

