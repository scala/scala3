// scalajs: --skip --pending

import language.experimental.namedTypeArguments
object Test extends App {
  // Types
  type F0 = [T] => List[T] => Option[T]
  type F1 = [F[_], G[_], T] => (F[T], F[T] => G[T]) => G[T]
  type F11 = [F[_[_]], G[_[_]], T[_]] => (F[T], [U[_]] => F[U] => G[U]) => G[T]
  type F2 = [T, U] => (T, U) => Either[T, U]

  // Terms
  val t0 = [T] => (ts: List[T]) => ts.headOption
  val t0a: F0 = t0
  assert(t0(List(1, 2, 3)) == Some(1))

  val t1 = [F[_], G[_], T] => (ft: F[T], f: F[T] => G[T]) => f(ft)
  val t1a: F1 = t1
  assert(t1(List(1, 2, 3), (ts: List[Int]) => ts.headOption) == Some(1))

  val t11 = [F[_[_]], G[_[_]], T[_]] => (fl: F[T], f: [U[_]] => F[U] => G[U]) => f(fl)
  val t11a: F11 = t11
  case class C11[F[_]](is: F[Int])
  case class D11[F[_]](is: F[Int])
  assert(t11[F = C11](C11(List(1, 2, 3)), [U[_]] => (c: C11[U]) => D11(c.is)) == D11(List(1, 2, 3)))

  val t2 = [T, U] => (t: T, u: U) => Left(t)
  val t2a: F2 = t2
  assert(t2(23, "foo") == Left(23))

  // Polymorphic idenity
  val pid = [T] => (t: T) => t

  // Method with poly function argument
  def m[T](f: [U] => U => U, t: T) = f(t)
  val m0 = m(pid, 23)

  // Constructor with poly function argument
  class C[T](f: [U] => U => U, t: T) { val v: T = f(t) }
  val c0 = new C(pid, 23)

  // Function with poly function argument
  val mf = (f: [U] => U => U, t: Int) => f(t)
  val mf0 = mf(pid, 23)

  // Poly function with poly function argument
  val pf = [T] => (f: [U] => U => U, t: T) => f(t)
  val pf0 = pf(pid, 23)

  // Poly function with AnyVal arguments
  val pf2 = [T] => (f: [U] => U => U, t: Int) => f(t)
  val pf20 = pf2(pid, 23)

  // Implment/override
  val phd = [T] => (ts: List[T]) => ts.headOption

  trait A {
    val is: List[Int]
    def m1(f: [T] => List[T] => Option[T]): Option[Int]
    def m2(f: [T] => List[T] => Option[T]): Option[Int] = f(is)
  }

  class B(val is: List[Int]) extends A {
    def m1(f: [T] => List[T] => Option[T]): Option[Int] = f(is)
    override def m2(f: [T] => List[T] => Option[T]): Option[Int] = f(is)
  }

  assert(new B(List(1, 2, 3)).m1(phd) == Some(1))
  assert(new B(List(1, 2, 3)).m2(phd) == Some(1))

  // Overload
  class O(is: List[Int]) {
    def m(f: [T] => List[T] => Option[T]): (Option[Int], Boolean) = (f(is), true)
    def m(f: [T] => (List[T], T) => Option[T]): (Option[Int], Boolean) = (is.headOption.flatMap(f(is, _)), false)
  }

  assert(new O(List(1, 2, 3)).m(phd) == (Some(1), true))
  assert(new O(List(1, 2, 3)).m([T] => (ts: List[T], t: T) => Some(t)) == (Some(1), false))

  // Dependent
  trait Entry[V] { type Key; val key: Key ; val value: V }
  def extractKey[V](e: Entry[V]): e.Key = e.key
  val md = [V] => (e: Entry[V]) => extractKey(e)
  val eis = new Entry[Int] { type Key = String ; val key = "foo" ; val value = 23 }
  val v0 = md(eis)
  val v0a: String = v0
  assert(v0 == "foo")

  // Contextual
  trait Show[T] { def show(t: T): String }
  implicit val si: Show[Int] =
    new Show[Int] {
      def show(t: Int): String = t.toString
    }
  val s = [T] => (t: T) => (st: Show[T]) ?=> st.show(t)
  assert(s(23) == "23")

  // Parens handling
  val tt1: [T] => (T => T) = [T] =>  (x: T) => x
  val tt2: [T] =>  T => T =  [T] => ((x: T) => x)
  val tt3: [T] =>  T => T =  [T] => { (x: T) => x }
  val tt4: [T] =>  T => T =  [T] => (x: T) => { x }
}
