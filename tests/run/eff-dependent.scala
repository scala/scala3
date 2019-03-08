object Test extends App {

  trait Effect

  // Type X => Y
  abstract class Fun[-X, +Y] {
    type Eff <: Effect
    def apply(x: X): given Eff => Y
  }

  class CanThrow extends Effect
  class CanIO extends Effect

  val i2s = new Fun[Int, String] { type Eff = CanThrow; def apply(x: Int) = x.toString }
  val s2i = new Fun[String, Int] { type Eff = CanIO; def apply(x: String) = x.length }

  implicit val ct: CanThrow = new CanThrow
  implicit val ci: CanIO = new CanIO

  // def map(f: A => B)(xs: List[A]): List[B]
  def map[A, B](f: Fun[A, B])(xs: List[A]): given f.Eff => List[B] =
    xs.map(f.apply)

  // def mapFn[A, B]: (A => B) -> List[A] -> List[B]
  def mapFn[A, B]: (f: Fun[A, B]) => List[A] => given f.Eff => List[B] =
    f => xs => map(f)(xs)

  // def compose(f: A => B)(g: B => C)(x: A): C
  def compose[A, B, C](f: Fun[A, B])(g: Fun[B, C])(x: A): given f.Eff => given g.Eff => C = g(f(x))

  // def composeFn: (A => B) -> (B => C) -> A -> C
  def composeFn[A, B, C]: (f: Fun[A, B]) => (g: Fun[B, C]) => A => given f.Eff => given g.Eff => C =
    f => g => x => compose(f)(g)(x)

  assert(mapFn(i2s)(List(1, 2, 3)).mkString == "123")
  assert(composeFn(i2s)(s2i)(22) == 2)

}
