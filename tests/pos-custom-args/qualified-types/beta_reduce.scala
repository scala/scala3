// Test that lambda beta reduction works in the EGraph.

// Single-argument lambda, constructor field access
case class Box[T](p: T => Boolean):
  def get: {res: T with p(res)} = ???

val box: Box[Int] with (box == Box[Int](x => x > 0)) = Box[Int](x => x > 0)
val v: {res: Int with res > 0} = box.get

// Multi-argument lambda, constructor field access
case class Pair[A, B](p: (A, B) => Boolean):
  def check(a: A, b: B): {res: Boolean with res == p(a, b)} = ???

val pair: Pair[Int, Int] with (pair == Pair[Int, Int]((x, y) => x < y)) =
  Pair[Int, Int]((x, y) => x < y)
val c: {res: Boolean with res == (1 < 2)} = pair.check(1, 2)

// Lambda with equality predicate
case class Refined[T](p: T => Boolean):
  def get: {res: T with p(res)} = ???

val r: Refined[Int] with (r == Refined[Int](x => x == 42)) = Refined[Int](x => x == 42)
val w: {res: Int with res == 42} = r.get

// Nested lambda: outer param referenced inside inner lambda body
case class Outer[T](p: T => T => Boolean):
  def check(a: T, b: T): {res: Boolean with res == p(a)(b)} = ???

val outer: Outer[Int] with (outer == Outer[Int](x => y => x < y)) =
  Outer[Int](x => y => x < y)
val nested: {res: Boolean with res == (1 < 2)} = outer.check(1, 2)
