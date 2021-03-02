object test0 {
  object :+ {
    def unapply[T](l: ::[T]): (List[T], T) = (l.tail, l.head)
  }

  def f(xs: List[Int]) =
    xs match
    case init :+ last => ()
    case Nil => ()
}

object test1 {
  import scala.annotation.covers

  type /[T, U] = T @covers[U]

  object :+ {
    def unapply[T](l: List[T]): Option[(List[T], T)] / ::[T] = ???
  }

  def f(xs: List[Int]) =
    xs match
    case init :+ last => ()
    case Nil => ()
}

object test2lib {
  import scala.annotation.covers

  class ValDef
  class TypeDef
  opaque type ValDefs  <: List[ValDef]  = List[ValDef]
  opaque type TypeDefs <: List[TypeDef] = List[TypeDef]

  type ParamClause = ValDefs | TypeDefs

  object ValDefs:
    def unapply(pc: ParamClause): Option[ValDefs] @covers[ValDefs] = ??? // matches empty list and all lists of ValDefs

    def apply(vals: List[ValDef]): ValDefs = vals

  object TypeDefs:
    def unapply(pc: ParamClause): Option[TypeDefs] @covers[TypeDefs] = ??? // matches non-empty lists of TypeDefs

    def apply(tdefs: List[TypeDef]): TypeDefs =
      assert(tdefs.nonEmpty)
      tdefs
}

object test2 {
  import test2lib._

  def f(pc: ParamClause) =
    pc match
    case ValDefs(vs) => ()
    case TypeDefs(ts) => ()
}

object test3lib {
  import scala.annotation.covers

  opaque type Nat <: Int = Int
  opaque type Neg <: Int = Int

  type Num = Nat | Neg

  object Nat:
    def unapply(x: Num): Option[Nat] @covers[Nat] =
      if x >= 0 then Some(x) else None

    def apply(x: Int): Nat =
      assert(x >= 0)
      x

  object Neg:
    def unapply(x: Num): Option[Neg] @covers[Neg] =
      if x < 0 then Some(x) else None

    def apply(x: Int): Nat =
      assert(x < 0)
      x
}

object test3 {
  import test3lib._

  def foo(x: Num) =
    x match
    case Nat(x) =>
    case Neg(x) =>
}

object test4 {
  import scala.reflect.TypeTest

  def test[A](a: A | String)(using TypeTest[A | String, A]) =
    a match
    case a: A =>
    case s: String =>
}