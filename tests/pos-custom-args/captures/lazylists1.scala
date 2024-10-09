class CC
type Cap = CC^

trait LazyList[+A]:

  def isEmpty: Boolean
  def head: A
  def tail: LazyList[A]^{this}
  def concat[B >: A](other: LazyList[B]^): LazyList[B]^{this, other}

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???
  def concat[B](other: LazyList[B]^): LazyList[B]^{other} = other

def LazyCons[A, C^](x: A)(xs: () => LazyList[A]^{C^}): LazyList[A]^{xs, C^} = new LazyList[A]:
  def isEmpty = false
  def head = x
  def tail: LazyList[A]^{this} = xs()
  def concat[B >: A](other: LazyList[B]^): LazyList[B]^{this, other} =
    LazyCons(head)(() => tail.concat(other))

extension [A](xs: LazyList[A]^)
  def map[B](f: A => B): LazyList[B]^{xs, f} =
    if xs.isEmpty then LazyNil
    else LazyCons(f(xs.head))(() => xs.tail.map(f))

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  def g(x: String): String = if cap2 == cap2 then "" else "a"

  val xs = LazyCons("")(() => if f("") == f("") then LazyNil else LazyNil)
  val xsc: LazyList[String]^{cap1} = xs
  val ys = xs.map(g)
  val ysc: LazyList[String]^{cap1, cap2} = ys
  val zs = LazyCons("")(() => if g("") == g("") then LazyNil else LazyNil)
  val as = xs.concat(zs)
  val asc: LazyList[String]^{xs, zs} = as
