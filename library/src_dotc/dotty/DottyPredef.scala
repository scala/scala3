package dotty

import scala.reflect.ClassTag
import scala.Predef.???
import scala.collection.Seq

/** unimplemented implicit for TypeTag */
object DottyPredef {
  /** A fall-back implicit to compare values of any types.
   *  The compiler will restrict implicit instances of `eqAny`. An instance
   *  `eqAny[T, U]` is _valid_ if `T <: U` or `U <: T` or both `T` and `U` are
   *  Eq-free. A type `S` is Eq-free if there is no implicit instance of `Eq[S, S]`.
   *  An implicit search will fail instead of returning an invalid `eqAny` instance.
   */
  implicit def eqAny[L, R]: Eq[L, R] = Eq

  implicit def eqNumber   : Eq[Number, Number] = Eq
  implicit def eqString   : Eq[String, String] = Eq

  // true asymmetry, modeling the (somewhat problematic) nature of equals on Proxies
  implicit def eqProxy    : Eq[Proxy, Any]     = Eq

  implicit def eqSeq[T, U](implicit eq: Eq[T, U]): Eq[Seq[T], Seq[U]] = Eq
  implicit def eqByteNum  : Eq[Byte, Number]   = Eq
  implicit def eqNumByte  : Eq[Number, Byte]   = Eq
  implicit def eqCharNum  : Eq[Char, Number]   = Eq
  implicit def eqNumChar  : Eq[Number, Char]   = Eq
  implicit def eqShortNum : Eq[Short, Number]  = Eq
  implicit def eqNumShort : Eq[Number, Short]  = Eq
  implicit def eqIntNum   : Eq[Int, Number]    = Eq
  implicit def eqNumInt   : Eq[Number, Int]    = Eq
  implicit def eqLongNum  : Eq[Long, Number]   = Eq
  implicit def eqNumLong  : Eq[Number, Long]   = Eq
  implicit def eqFloatNum : Eq[Float, Number]  = Eq
  implicit def eqNumFloat : Eq[Number, Float]  = Eq
  implicit def eqDoubleNum: Eq[Double, Number] = Eq
  implicit def eqNumDouble: Eq[Number, Double] = Eq

  /** A class for implicit values that can serve as implicit conversions
   *  The implicit resolution algorithm will act as if there existed
   *  the additional implicit definition:
   *
   *    def $implicitConversion[T, U](x: T)(c: ImplicitConverter[T, U]): U = c(x)
   *
   *  However, the presence of this definition would slow down implicit search since
   *  its outermost type matches any pair of types. Therefore, implicit search
   *  contains a special case in `Implicits#discardForView` which emulates the
   *  conversion in a more efficient way.
   *
   *  Note that this is a SAM class - function literals are automatically converted
   *  to `ImplicitConverter` values.
   *
   *  Also note that in bootstrapped dotty, `Predef.<:<` should inherit from
   *  `ImplicitConverter`. This would cut the number of special cases in
   *  `discardForView` from two to one.
   */
  abstract class ImplicitConverter[-T, +U] extends Function1[T, U]

  import dotty.{TupleCons => TC}

  implicit class ArrowAssoc[A](a: A) {
    def -> [B](b: B): TC[A, TC[B, Unit]] = TC(a, TC(b, ()))
  }

  implicit class Tuple1Assessors[A](l: TC[A, Unit]) {
    def _1 = l match { case TC(x, _) => x }
  }

  type Tuple1[A] = TC[A, Unit]

  def Tuple1[A](a: A): TC[A, Unit] = TC(a, ())

  implicit class Tuple2Assessors[A, B](l: TC[A, TC[B, Unit]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
  }

  type Tuple2[A, B] = TC[A, TC[B, Unit]]

  def Tuple2[A, B](a: A, b: B): TC[A, TC[B, Unit]] = TC(a, TC(b, ()))

  implicit class Tuple3Assessors[A, B, C](l: TC[A, TC[B, TC[C, Unit]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
  }

  type Tuple3[A, B, C] = TC[A, TC[B, TC[C, Unit]]]

  def Tuple3[A, B, C](a: A, b: B, c: C): TC[A, TC[B, TC[C, Unit]]] = TC(a, TC(b, TC(c, ())))

  implicit class Tuple4Assessors[A, B, C, D](l: (A, B, C, D)) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
  }

  type Tuple4[A, B, C, D] = TC[A, TC[B, TC[C, TC[D, Unit]]]]

  def Tuple4[A, B, C, D](a: A, b: B, c: C, d: D): TC[A, TC[B, TC[C, TC[D, Unit]]]] = TC(a, TC(b, TC(c, TC(d, ()))))

  implicit class Tuple5Assessors[A, B, C, D, E](l: TC[A, TC[B, TC[C, TC[D, TC[E, Unit]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
  }

  type Tuple5[A, B, C, D, E] = TC[A, TC[B, TC[C, TC[D, TC[E, Unit]]]]]

  def Tuple5[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E): TC[A, TC[B, TC[C, TC[D, TC[E, Unit]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, ())))))

  implicit class Tuple6Assessors[A, B, C, D, E, F](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, Unit]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
  }

  type Tuple6[A, B, C, D, E, F] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, Unit]]]]]]

  def Tuple6[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, Unit]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, ()))))))

  implicit class Tuple7Assessors[A, B, C, D, E, F, G](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, Unit]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
  }

  type Tuple7[A, B, C, D, E, F, G] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, Unit]]]]]]]

  def Tuple7[A, B, C, D, E, F, G](a: A, b: B, c: C, d: D, e: E, f: F, g: G): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, Unit]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, ())))))))

  implicit class Tuple8Assessors[A, B, C, D, E, F, G, H](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, Unit]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
  }

  type Tuple8[A, B, C, D, E, F, G, H] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, Unit]]]]]]]]

  def Tuple8[A, B, C, D, E, F, G, H](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, Unit]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, ()))))))))

  implicit class Tuple9Assessors[A, B, C, D, E, F, G, H, I](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, Unit]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
  }

  type Tuple9[A, B, C, D, E, F, G, H, I] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, Unit]]]]]]]]]

  def Tuple9[A, B, C, D, E, F, G, H, I](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, Unit]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, ())))))))))

  implicit class Tuple10Assessors[A, B, C, D, E, F, G, H, I, J](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, Unit]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
  }

  type Tuple10[A, B, C, D, E, F, G, H, I, J] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, Unit]]]]]]]]]]

  def Tuple10[A, B, C, D, E, F, G, H, I, J](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, Unit]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, ()))))))))))

  implicit class Tuple11Assessors[A, B, C, D, E, F, G, H, I, J, K](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, Unit]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
  }

  type Tuple11[A, B, C, D, E, F, G, H, I, J, K] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, Unit]]]]]]]]]]]

  def Tuple11[A, B, C, D, E, F, G, H, I, J, K](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, Unit]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, ())))))))))))

  implicit class Tuple12Assessors[A, B, C, D, E, F, G, H, I, J, K, L](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, Unit]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
  }

  type Tuple12[A, B, C, D, E, F, G, H, I, J, K, L] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, Unit]]]]]]]]]]]]

  def Tuple12[A, B, C, D, E, F, G, H, I, J, K, L](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, Unit]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, ()))))))))))))

  implicit class Tuple13Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, Unit]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
  }

  type Tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, Unit]]]]]]]]]]]]]

  def Tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, Unit]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, ())))))))))))))

  implicit class Tuple14Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, Unit]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
  }

  type Tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, Unit]]]]]]]]]]]]]]

  def Tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, Unit]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, ()))))))))))))))

  implicit class Tuple15Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, Unit]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
  }

  type Tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, Unit]]]]]]]]]]]]]]]

  def Tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, Unit]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, ())))))))))))))))

  implicit class Tuple16Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, Unit]]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
    def _16 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))) => x }
  }

  type Tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, Unit]]]]]]]]]]]]]]]]

  def Tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, Unit]]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, TC(p, ()))))))))))))))))

  implicit class Tuple17Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, Unit]]]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
    def _16 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))) => x }
    def _17 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))) => x }
  }

  type Tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, Unit]]]]]]]]]]]]]]]]]

  def Tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, Unit]]]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, TC(p, TC(q, ())))))))))))))))))

  implicit class Tuple18Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, Unit]]]]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
    def _16 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))) => x }
    def _17 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))) => x }
    def _18 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))) => x }
  }

  type Tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, Unit]]]]]]]]]]]]]]]]]]

  def Tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, Unit]]]]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, TC(p, TC(q, TC(r, ()))))))))))))))))))

  implicit class Tuple19Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, Unit]]]]]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
    def _16 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))) => x }
    def _17 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))) => x }
    def _18 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))) => x }
    def _19 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))))) => x }
  }

  type Tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, Unit]]]]]]]]]]]]]]]]]]]

  def Tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, Unit]]]]]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, TC(p, TC(q, TC(r, TC(s, ())))))))))))))))))))

  implicit class Tuple20Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, Unit]]]]]]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
    def _16 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))) => x }
    def _17 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))) => x }
    def _18 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))) => x }
    def _19 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))))) => x }
    def _20 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))))) => x }
  }

  type Tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, Unit]]]]]]]]]]]]]]]]]]]]

  def Tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, Unit]]]]]]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, TC(p, TC(q, TC(r, TC(s, TC(t, ()))))))))))))))))))))

  implicit class Tuple21Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, TC[U, Unit]]]]]]]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
    def _16 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))) => x }
    def _17 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))) => x }
    def _18 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))) => x }
    def _19 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))))) => x }
    def _20 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))))) => x }
    def _21 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))))))) => x }
  }

  type Tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, TC[U, Unit]]]]]]]]]]]]]]]]]]]]]

  def Tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, TC[U, Unit]]]]]]]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, TC(p, TC(q, TC(r, TC(s, TC(t, TC(u, ())))))))))))))))))))))

  implicit class Tuple22Assessors[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](l: TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, TC[U, TC[V, Unit]]]]]]]]]]]]]]]]]]]]]]) {
    def _1 = l match { case TC(x, _) => x }
    def _2 = l match { case TC(_, TC(x, _)) => x }
    def _3 = l match { case TC(_, TC(_, TC(x, _))) => x }
    def _4 = l match { case TC(_, TC(_, TC(_, TC(x, _)))) => x }
    def _5 = l match { case TC(_, TC(_, TC(_, TC(_, TC(x, _))))) => x }
    def _6 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))) => x }
    def _7 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))) => x }
    def _8 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))) => x }
    def _9 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))) => x }
    def _10 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))) => x }
    def _11 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))) => x }
    def _12 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))) => x }
    def _13 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))) => x }
    def _14 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))) => x }
    def _15 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))) => x }
    def _16 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))) => x }
    def _17 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))) => x }
    def _18 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))) => x }
    def _19 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))))) => x }
    def _20 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))))) => x }
    def _21 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _))))))))))))))))))))) => x }
    def _22 = l match { case TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(_, TC(x, _)))))))))))))))))))))) => x }
  }

  type Tuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, TC[U, TC[V, Unit]]]]]]]]]]]]]]]]]]]]]]

  def Tuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V): TC[A, TC[B, TC[C, TC[D, TC[E, TC[F, TC[G, TC[H, TC[I, TC[J, TC[K, TC[L, TC[M, TC[N, TC[O, TC[P, TC[Q, TC[R, TC[S, TC[T, TC[U, TC[V, Unit]]]]]]]]]]]]]]]]]]]]]] = TC(a, TC(b, TC(c, TC(d, TC(e, TC(f, TC(g, TC(h, TC(i, TC(j, TC(k, TC(l, TC(m, TC(n, TC(o, TC(p, TC(q, TC(r, TC(s, TC(t, TC(u, TC(v, ()))))))))))))))))))))))
}
