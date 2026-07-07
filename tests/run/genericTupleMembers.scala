
// Member selection on a generic tuple hidden behind an abstract type bound
// (an abstract type member, or a type parameter), where the bound itself
// resolves to a generic `*:` tuple rather than a literal TupleN. See i26099.
trait Container:
  type A <: (Int *: String *: EmptyTuple)
  def get: A

class ContainerImpl extends Container:
  type A = Int *: String *: EmptyTuple
  def get: A = 1 *: "s" *: EmptyTuple

trait ContainerG[T <: Tuple]:
  type A <: T
  def get: A

class ContainerGImpl extends ContainerG[Int *: String *: EmptyTuple]:
  type A = Int *: String *: EmptyTuple
  def get: A = 1 *: "s" *: EmptyTuple

def testAbstractTupleBound(c: Container): Unit =
  assert(c.get._1 == 1)
  assert(c.get._2 == "s")

def testAbstractTupleBoundGeneric(c: ContainerG[Int *: String *: EmptyTuple]): Unit =
  assert(c.get._1 == 1)
  assert(c.get._2 == "s")

@main def Test: Unit =
  val tup1 = 1 *: EmptyTuple
  val tup2 = 1 *: 2 *: EmptyTuple
  val tup3 = 1 *: 2 *: 3 *: EmptyTuple
  val tup4 = 1 *: 2 *: 3 *: 4 *: EmptyTuple
  val tup5 = 1 *: 2 *: 3 *: 4 *: 5 *: EmptyTuple
  val tup22 = 1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: EmptyTuple

  tup1._1

  tup2._1
  tup2._2
  tup2.swap

  tup3._1
  tup3._2
  tup3._3

  tup4._1
  tup4._2
  tup4._3
  tup4._4

  tup22._1
  tup22._2
  tup22._3
  tup22._4
  tup22._5
  tup22._6
  tup22._7
  tup22._8
  tup22._9
  tup22._10
  tup22._11
  tup22._12
  tup22._13
  tup22._14
  tup22._15
  tup22._16
  tup22._17
  tup22._18
  tup22._19
  tup22._20
  tup22._21
  tup22._22

  testAbstractTupleBound(ContainerImpl())
  testAbstractTupleBoundGeneric(ContainerGImpl())
