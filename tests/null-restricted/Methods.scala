// Test case for MethodOrPoly handling in jsig
// jsig matches: case mtd: MethodOrPoly

class MethodTypesTest:
  // Simple method
  def simpleMethod(x: Int): String = x.toString

  // Method with multiple parameters
  def multiParam(a: Int, b: String, c: Double): Boolean =
    true

  // Method with multiple parameter groups (curried)
  def curried(a: Int)(b: String): Double =
    a.toDouble + b.length

  // Method with type parameters
  def generic[T, U](t: T, u: U): (U, T) =
    (u, t)

  // Method with type parameters and bounds
  def withBounds[T <: Comparable[T]](t: T): T =
    t

  // Method returning function
  def returnFunction(): (Int => String) =
    _.toString

  // Method with by-name parameter
  def byName(f: => Int): Int = f + 1

  // Method with var parameter
  def varArgs(xs: Int*): Int = xs.sum

  // Method with default parameter
  def withDefault(x: Int = 42): Int = x

  // Method with implicit parameter
  def implicitMethod(x: Int)(implicit s: String): String =
    x.toString + s

  // Method with context parameter (Scala 3)
  def contextMethod(x: Int)(using s: String): String =
    x.toString + s

  // Method with generic type parameter with context bound
  def withContextBound[T: Ordering](t: T): T = t

  // Method returning generic type
  def returnGeneric[T](t: T): Option[T] = Some(t)

  // Method with intersection type
  def intersection[T <: (Comparable[T] & Serializable)](t: T): T = t

  // Method with union type return
  def unionReturn(b: Boolean): Int | String =
    if b then 42 else "answer"

  // Method with refined type parameter
  def refinedParam(x: List[Int] { def isEmpty: Boolean }): Int =
    x.size
