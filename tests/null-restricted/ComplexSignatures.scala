// Combined test cases for jsig with various complex type combinations

class ComplexSignaturesTest:
  // Combine refined type with type parameter
  def refinedGeneric[T <: List[Int] { def head: Int }](x: T): Int =
    x.head

  // Type parameter with refined bound
  def refinedBound[T <: (List[String] { def isEmpty: Boolean })]:
    T = null.asInstanceOf[T]

  // Union type with refined type
  def unionRefined(): (List[Int] { def size: Int }) | String =
    List(1, 2, 3)

  def unionRefined2(): (List[Int] { def size: Int }) | String =
    "List"

  // Intersection with refined type
  trait Refined:
    def refine: List[Int] { def isEmpty: Boolean }

  def intersectionRefined(): Refined & java.io.Serializable =
    new Refined with java.io.Serializable:
      def refine = List()

  // Array of generic type with bounds
  def arrayOfBounded[T <: Comparable[T]]: Array[T] =
    null.asInstanceOf[Array[T]]

  // Method with complex parameter list
  def complexParams[A <: List[B], B](
    a: A,
    b: => B,
    c: B | Null,
    d: List[B] { def isEmpty: Boolean }
  ): (A, B, Option[B]) =
    (a, b, None)

  // Curried method with type parameters
  def curryGeneric[A, B](a: A)(b: B)(c: => A): (A, B) =
    (a, b)

  // By-name returning function type
  def byNameFunc(f: => (Int => String)): String =
    f(42)

  def byNameFunc1(f: => ((Int => String) | Null)): String =
    val g = f
    if (g != null) then g(42) else "NULL"

  // Generic with union and intersection
  def complexGeneric[T <: (Comparable[T] & java.io.Serializable)]:
    (T | Null) =
    null

  // Singleton type with generic
  def singletonGeneric[T](x: T): x.type = x

  def useSingletonGeneric(): String =
    singletonGeneric("test")

  // Nested method types
  def methodReturningMethod(): (Int => (String => Boolean)) =
    i => s => true

  def methodTakingMethod(f: (Int => (String => Boolean))): Boolean =
    f(1)("test")

  // Option of union type
  def optionUnion(): Option[Int | String] =
    Some(42)

  // List of intersection type
  trait Reader:
    def read(): String

  trait Closer:
    def close(): Unit

  def listIntersection(): List[Reader & Closer] =
    List()

  // Generic variance combination
  trait Producer[+T]:
    def produce: T

  trait Consumer[-T]:
    def consume(t: T): Unit

  def variance(): (Producer[String], Consumer[Int]) =
    ???

  // Refined type in function signature
  def functionWithRefined(
    f: (List[Int] { def head: Int }) => String
  ): String =
    f(List(1, 2))

  // Type parameter with multiple bounds and context
  def multiConstraint[T <: Comparable[T]](t: T)(using ord: Ordering[T]): T = t

  // Polymorphic recursive type
  def recursive[T](t: T): List[T] = List(t)

  def useRecursive(): List[List[List[Int]]] =
    recursive(recursive(recursive(42)))

class Main:
  def main(args: Array[String]): Unit = {
    val test = new ComplexSignaturesTest
    // test.useRecursive()
    test.byNameFunc1(null)
    // val res = test.methodReturningMethod()
  }