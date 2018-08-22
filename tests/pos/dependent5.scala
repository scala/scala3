object IdrisVect {
  dependent def þ [T] : T = ??? : T

  sealed trait Nat { val pred: Nat }
  dependent case object Zero extends Nat { val pred: Nat = Zero }
  dependent case class Succ(pred: Nat) extends Nat

  // case class Fin(n: Nat, m: Nat, ev: [n < m])

  sealed trait Fin { def bound: Nat }
  dependent case class FinZero(val bound: Succ) extends Fin
  dependent case class FinSucc(f: Fin) extends Fin {
    val bound: { Succ(f.bound) } = Succ(f.bound)
  }

  object Nat {
    type _0 = { Zero }
    type _1 = { Succ(Zero) }
    type _2 = { Succ(Succ(Zero)) }
    type _3 = { Succ(Succ(Succ(Zero))) }
  }
  import Nat._

  sealed trait  Vect[T] { def length: Nat }
  dependent case class Nil[T](length: Zero.type) extends Vect[T]
  dependent case class Cons[T](head: T, tail: Vect[T], length: Nat) extends Vect[T]

  object Vect {
    dependent def sized[T](n: Nat) = Cons(þ[T], þ[Vect[T]], n)
    dependent def nil = Nil[Int](Zero)
    dependent def cons(head: Int, tail: Vect[Int]): Vect[Int] = Cons(head, tail, Succ(tail.length))
  }
  import Vect._

  val y0: _0 = nil.length
  val y1: _1 = cons(1, nil).length
  val y2: _2 = cons(1, cons(2, nil)).length
  val y3: _3 = cons(1, cons(2, cons(3, nil))).length

  dependent def concat(v1: Vect[Int], v2: Vect[Int]): Vect[Int] = {
    if (v1.isInstanceOf[Nil[Int]]) v2
    else {
      val vv1 = v1.asInstanceOf[Cons[Int]]
      cons(vv1.head, concat(vv1.tail, v2))
    }
  }

  val x1: { nil }                            = concat(nil, nil)
  val x2: { cons(1, nil) }                   = concat(cons(1, nil), nil)
  val x3: { cons(1, nil) }                   = concat(nil, cons(1, nil))
  val x4: { cons(1, cons(2, nil)) }          = concat(cons(1, nil), cons(2, nil))
  val x5: { cons(1, cons(2, cons(3, nil))) } = concat(cons(1, nil), cons(2, cons(3, nil)))


  val x1b: { Nil(þ[Zero.type]) } = concat(nil, nil)
  val x5b: { Cons(þ, þ, þ[_3]) } = concat(cons(1, nil), cons(2, cons(3, nil)))
  val x5c: { sized(þ[_3]) }      = concat(cons(1, nil), cons(2, cons(3, nil)))

  /** Calculate the length of a `Vect`. */
  dependent def length[T](xs: Vect[T]): Nat =
    if (xs.isInstanceOf[Nil[_]]) Zero
    else {
      val xs1 = xs.asInstanceOf[Cons[_]].tail
      Succ(length(xs1))
    }

  val l1_member: _0 = x1.length
  val l2_member: _1 = x2.length
  val l3_member: _1 = x3.length
  val l4_member: _2 = x4.length
  val l5_member: _3 = x5.length

  val l1: _0 = length(x1)
  val l2: _1 = length(x2)
  val l3: _1 = length(x3)
  val l4: _2 = length(x4)
  val l5: _3 = length(x5)

  // val _: { Vect.sized[Int](Succ(þ[Nat])) } = "abc"
  def f[T](x: { Vect.sized[T](Succ(þ[Nat])) }, y: Int) = ???
  // f(x2)
  // /** All but the first element of the vector */
  // dependent def tail[T](v: { Vect.sized[T](Succ(þ[Nat])) }): Vect[T] =
  //   v.asInstanceOf[Cons[T]].tail

  // // val t1: { nil }                   = tail(x1) // error: stuck on failing asInstanceOf, as expected!
  // val t2: { nil }                   = tail(x2)
  // val t3: { nil }                   = tail(x3)
  // val t4: { cons(2, nil) }          = tail(x4)
  // val t5: { cons(2, cons(3, nil)) } = tail(x5)

  /** Only the first element of the vector */
  dependent def head[T](v: Vect[T]): T =
    v.asInstanceOf[Cons[T]].head

  // val h1: 1 = head[Int](x1) // error: stuck on failing asInstanceOf, as expected!
  val h2: 1 = head[Int](x2)
  val h3: 1 = head[Int](x3) // TODO: inference fucks it up when we don't put the explicit [Int]
  val h4: 1 = head[Int](x4)
  val h5: 1 = head[Int](x5)

  dependent def headSafe[T](v: { Vect.sized[T](Succ(þ[Nat])) }): T =
    v.asInstanceOf[Cons[T]].head

  // val hs1: 1 = headSafe[Int](x1) // error: not a subtype
  val hs2: 1 = headSafe[Int](x2)
  val hs3: 1 = headSafe[Int](x3) // TODO: inference fucks it up when we don't put the explicit [Int]
  val hs4: 1 = headSafe[Int](x4)
  val hs5: 1 = headSafe[Int](x5)

  def headSafeOpaque[T](v: { Vect.sized[T](Succ(þ[Nat])) }): T =
    v.asInstanceOf[Cons[T]].head

  // val hso1: Int = headSafeOpaque[Int](x1) // error: not a subtype
  val hso2: Int = headSafeOpaque[Int](x2)
  val hso3: Int = headSafeOpaque[Int](x3) // TODO: inference fucks it up when we don't put the explicit [Int]
  val hso4: Int = headSafeOpaque[Int](x4)
  val hso5: Int = headSafeOpaque[Int](x5)

  // TODO
  // def headSafeOpaquePrecise[T](v: { Vect.sized[T](Succ(þ[Nat])) }): { headSafe(v) } =
  //   v.asInstanceOf[Cons[T]].head

  // val hsop1: 1 = head[Int](x1) // error: not a subtype
  val hsop2: 1 = head[Int](x2)
  val hsop3: 1 = head[Int](x3) // TODO: inference fucks it up when we don't put the explicit [Int]
  val hsop4: 1 = head[Int](x4)
  val hsop5: 1 = head[Int](x5)

  /** The last element of the vector */
  dependent def last[T](v: Vect[T]): T = {
    val h = v.asInstanceOf[Cons[T]].head
    val t = v.asInstanceOf[Cons[T]].tail
    if (t.isInstanceOf[Nil[T]])
      h
    else
      last[T](t)
  }

  // val a1: 1 = last(x1) // error: stuck on failing asInstanceOf, as expected!
  val a2: 1 = last[Int](x2)
  val a3: 1 = last[Int](x3) // TODO: inference fucks it up when we don't put the explicit [Int]
  val a4: 2 = last[Int](x4)
  val a5: 3 = last[Int](x5)

  // /** Extract a particular element from a vector */
  // dependent def index(i: Nat, v: { sized(i) }): T
  // index FZ     (x::xs) = x
  // index (FS k) (x::xs) = index k xs

}

object IdrisVect2 {
  def length(x: Int) = {
    // At some point x became a shared tree in pickling
    val y = x
    identity(x)
  }
}

object MatchError129 {
  dependent def length[T](x: Unit): Unit = {
    val y = x
    length(y)
  }
}
