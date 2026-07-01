// Test case for OrNull (union) and AndType (intersection) handling in jsig
// jsig matches: case OrNull(tp1) and case tp: AndType

class UnionAndIntersectionTypesTest:
  // Simple union type
  def unionType(): Int | String = 42

  def unionParam(x: Int | String): Unit = ()

  // Union with null
  def unionWithNull(): String | Null = "hello"

  def nullableParam(x: String | Null): Unit = ()

  // Multiple type union
  def multiUnion(): Int | String | Double = 42

  def multiUnionParam(x: Int | String | Double): Unit = ()

  // Union return type with object
  class Container
  def unionObject(): Container | String = Container()

  def unionObjectParam(x: Container | String): Unit = ()

  // Union in generic
  def unionInGeneric(): List[Int | String] =
    List(1, "two", 3)

  def unionGenericParam(x: List[Int | String]): Unit = ()

  // Intersection type (trait/interface intersection)
  trait Comparable:
    def compareTo(other: Comparable): Int

  trait Serializable:
    def serialize(): String

  def intersectionParam(x: Comparable & Serializable): Unit = ()

  def intersectionReturn(): Comparable & Serializable =
    new Comparable with Serializable:
      def compareTo(other: Comparable): Int = 0
      def serialize(): String = "test"

  // Union with intersection
  def complexUnion(): (Int & String) | (Double & Serializable) =
    null.asInstanceOf[(Int & String) | (Double & Serializable)]

  // Intersection in generic
  def intersectionInGeneric[T <: (Comparable & Serializable)](t: T): T = t

  // Union in method parameter position
  def methodWithUnion(f: (Int | String) => String): String =
    f(42)

  // Nested unions
  def nestedUnion(): (Int | String) | (Double | Boolean) =
    42
