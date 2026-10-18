// Polymorphic functions without a term parameter section and other
// shapes made possible by representing `[T] => R` as `PolyFunction { def apply[T]: R }`.
object Test extends App {

  // Type argument application yields an ordinary function value
  val id: [T] => T => T = [T] => (x: T) => x
  val idInt: Int => Int = id[Int]
  assert(idInt(3) == 3)
  assert(id[String]("a") == "a")
  assert(id("b") == "b")

  // Inferred result type
  val pair = [T] => (x: T) => (x, x)
  val pairA: [T] => T => (T, T) = pair
  assert(pair(1) == (1, 1))

  // Nested polymorphic functions
  val curried: [T] => T => [U] => U => (T, U) =
    [T] => (x: T) => [U] => (y: U) => (x, y)
  assert(curried[Int](1)[String]("a") == (1, "a"))
  assert(curried(1)("a") == (1, "a"))

  // Context function results
  val ctx: [T] => List[T] ?=> Option[T] = [T] => (xs: List[T]) ?=> xs.headOption
  given List[Int] = List(1, 2, 3)
  assert(ctx[Int] == Some(1))

  // Function0 results
  val thunk: [T] => () => List[T] = [T] => () => Nil
  assert(thunk[Int]().isEmpty)

  // Poly functions as parameters and results
  def apply2[A, B](f: [T] => T => T, a: A, b: B): (A, B) = (f(a), f(b))
  assert(apply2(id, 1, "x") == (1, "x"))

  // Explicit type parameter with bounds and multiple type parameters
  val fst: [A <: AnyRef, B] => (A, B) => A = [A <: AnyRef, B] => (a: A, b: B) => a
  assert(fst("s", 1) == "s")

  // Body that is not a function literal
  var useId = true
  val other: [T] => T => T = [T] => (x: T) => x
  //!!!val pick: [T] => T => T = [T] => (if useId then id[T] else other[T])
  //!!!assert(pick(1) == 1)
  useId = false
  //!!!assert(pick[String]("b") == "b")

  // Dependent function result
  trait Box { type Elem; val elem: Elem }
  val unbox: [T] => (b: Box) => b.Elem = [T] => (b: Box) => b.elem
  val box = new Box { type Elem = String; val elem = "boxed" }
  assert(unbox[Int](box) == "boxed")

  // Context bounds on type parameters
  trait Ord[X] { def compare(x: X, y: X): Int }
  given Ord[Int]:
    def compare(x: Int, y: Int) = x - y
  val less: [X: Ord] => (X, X) => Boolean = [X: Ord] => (x: X, y: X) => summon[Ord[X]].compare(x, y) < 0
  assert(less(1, 2))
  val zero: [X: Ord] => X => Int = [X: Ord as ord] => (x: X) => ord.compare(x, x)
  assert(zero(3) == 0)
}
