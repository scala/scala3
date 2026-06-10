// Test case for RefOrAppliedType handling in jsig
// jsig matches: case RefOrAppliedType(sym, pre, args)

class AppliedTypesTest:
  // Generic class
  class Pair[A, B](val a: A, val b: B)

  def pairMethod(): Pair[Int, String] =
    Pair(42, "hello")

  def pairParam(p: Pair[Int, String]): Unit = ()

  // Nested generic types
  def nestedGenerics(): List[Map[String, Int]] =
    List(Map("a" -> 1, "b" -> 2))

  def nestedGenericsParam(x: List[Map[String, Int]]): Unit = ()

  // Generic with variance
  trait Producer[+T]:
    def produce: T

  def producerMethod(): Producer[String] =
    new Producer[String]:
      def produce = "test"

  def producerParam(p: Producer[String]): Unit = ()

  // Multiple type arguments
  def mapMethod(): scala.collection.immutable.Map[String, Int] =
    Map("a" -> 1)

  // Tuple types
  def tupleMethod(): (Int, String, Boolean) =
    (1, "two", true)

  def tupleParam(t: (Int, String, Boolean)): Unit = ()

  // Generic with specialized type
  def listInt(): List[Int] = List(1, 2, 3)

  def listString(): List[String] = List("a", "b")

  // Function type
  def functionType(): (Int => String) =
    _.toString

  def functionTypeParam(f: (Int => String)): Unit = ()

  // Option type
  def optionMethod(): Option[Int] = Some(42)

  def optionParam(o: Option[Int]): Unit = ()

  // Either type
  def eitherMethod(): Either[String, Int] =
    Right(42)

  def eitherParam(e: Either[String, Int]): Unit = ()

  // Custom generic with bounds
  class Bounded[T <: Comparable[T]](val value: T)

  def boundedMethod(): Bounded[String] =
    Bounded("test")

  def boundedParam(b: Bounded[String]): Unit = ()

  // Deeply nested
  def deeplyNested(): Map[String, List[Option[Int]]] =
    Map("key" -> List(Some(1), None))
