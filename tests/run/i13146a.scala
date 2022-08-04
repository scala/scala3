import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

// File that breaks the infinite loop caused by implicit search in i13146.scala

inline def summonAll[P, T <: Tuple]: List[Eq[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => loopBreaker[P, t] :: summonAll[P, ts]

/** loopBreaker stops summoning a derived typeclass instance from inside its own definition
 *  @note aparently it needs to be defined separately from `summonAll` to avoid an infinite loop
 *  in inlining.
 */
inline def loopBreaker[P, T]: Eq[T] = compiletime.summonFrom {
  case infiniteRecursion: (T =:= P) => compiletime.error("cannot derive Eq, it will cause an infinite loop")
  case recursiveEvidence: (T <:< P) =>
    // summonInline will work because to get here `P` must also have a Mirror instance
    Eq.derived[T](using summonInline[Mirror.Of[T]])

  case existing: Eq[T] => existing
}

trait Eq[-T]:
  def eqv(x: T, y: T): Boolean

object Eq:

  given Eq[Int] with
    def eqv(x: Int, y: Int) = x == y

  def check(elem: Eq[_])(x: Any, y: Any): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[_]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(elems(ordx))(x, y)

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[_]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        iterator(x).zip(iterator(y)).zip(elems.iterator).forall {
          case ((x, y), elem) => check(elem)(x, y)
        }

  inline given derived[T](using m: Mirror.Of[T]): Eq[T] =
    lazy val elemInstances = summonAll[T, m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
end Eq

enum Opt[+T] derives Eq:
  case Sm(t: T)
  case Nn

case class Rat[N](n: N, d: Opt[N]) derives Eq

// Loop is impossible to derive generically, uncommenting will be an error.
// case class Loop(prev: Loop) derives Eq
// object Loop:
//   val Zero = Loop(null) // just to demonstrate that this cannot be derived generically

case class Nat(prev: Opt[Nat]) derives Eq

enum Nat1 derives Eq:
  case Succ(prev: Nat1) // this recursion is ok, because the parent type will be Succ
  case Zero

@main def Test(): Unit =
  import Opt.*
  val eqoi = summon[Eq[Opt[Int]]]
  assert(eqoi.eqv(Sm(23), Sm(23)))
  assert(!eqoi.eqv(Sm(23), Sm(13)))
  assert(!eqoi.eqv(Sm(23), Nn))

  // check that Rat.derived$Eq reuses Opt.derived$Eq
  val eqri = summon[Eq[Rat[Int]]]
  assert(eqri.eqv(Rat(23, Sm(23)), Rat(23, Sm(23))))
  assert(!eqri.eqv(Rat(23, Sm(23)), Rat(23, Nn)))
  assert(!eqri.eqv(Rat(23, Sm(23)), Rat(23, Sm(13))))

  // val eql = summon[Eq[Loop]]

  val eqn = summon[Eq[Nat]]
  assert(eqn.eqv(Nat(Nn), Nat(Nn)))
  assert(!eqn.eqv(Nat(Nn), Nat(Sm(Nat(Nn)))))

  val eqn1 = summon[Eq[Nat1]]
  assert(eqn1.eqv(Nat1.Succ(Nat1.Zero), Nat1.Succ(Nat1.Zero)))
  assert(!eqn1.eqv(Nat1.Succ(Nat1.Zero), Nat1.Succ(Nat1.Succ(Nat1.Zero))))
