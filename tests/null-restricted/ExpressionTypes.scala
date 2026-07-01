// Test case for ExprType handling in jsig
// jsig matches: case ExprType(restpe) if level == 0

class ExpressionTypesTest:
  // By-name parameter
  def byNameParam(f: => Int): Int = f

  def callByName(): Int = byNameParam(42)

  // By-name with string
  def byNameString(f: => String): String = f

  def callByNameString(): String = byNameString("hello")

  // By-name with generic type
  def byNameGeneric[T](f: => T): T = f

  def callByNameGeneric(): List[Int] =
    byNameGeneric(List(1, 2, 3))

  // By-name as return type (same as => T)
  def returnByName(): (=> Int) = 42

  // Method type from by-name
  def methodTypeFromByName(f: => List[String]): List[String] =
    f

  // Nested by-name
  def nestedByName(f: => (() => Int)): Int =
    f()()

  // By-name in collection type
  def byNameInMap(f: => String): scala.collection.mutable.Map[String, String] =
    scala.collection.mutable.Map("key" -> f)

  // Multiple by-name parameters
  def multiByName(a: => Int, b: => String): String =
    a.toString + b

  // By-name with class type
  class Container:
    val value: Int = 42

  def byNameContainer(f: => Container): Int =
    f.value
