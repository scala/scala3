// Test case for value class handling in jsig
// jsig matches: else if (sym.isPrimitiveValueClass) and else if (sym.isDerivedValueClass)

// Value class wrapping Int
class UserId(val value: Int) extends AnyVal:
  def increment(): UserId = UserId(value + 1)

// Value class wrapping String
class Email(val value: String) extends AnyVal:
  def domain(): String = value.split("@").last

// Value class with generic
class Holder[T](val value: T) extends AnyVal

class ValueClassesTest:
  // Value class as parameter
  def processUserId(id: UserId): Int = id.value

  def processEmail(email: Email): String = email.value

  // Value class as return type
  def makeUserId(v: Int): UserId = UserId(v)

  def makeEmail(v: String): Email = Email(v)

  // Generic value class
  def holdValue[T](v: Holder[T]): T = v.value

  def makeHolder[T](v: T): Holder[T] = Holder(v)

  // Multiple value class parameters
  def combine(id: UserId, email: Email): String =
    s"${id.value}:${email.value}"

  // Value class in collection
  def userIds(): List[UserId] =
    List(UserId(1), UserId(2), UserId(3))

  // Array of value class
  def userIdArray(): Array[UserId] =
    Array(UserId(1), UserId(2))

  // Optional value class
  def optionalUserId(): Option[UserId] =
    Some(UserId(42))

  // Value class with union type
  def userOrEmail(): UserId | Email =
    UserId(1)

  // Value class generic with type parameter
  def genericWithValueClass[T](h: Holder[T]): T =
    h.value

  // Nested value class
  def valueInList(): List[List[UserId]] =
    List(List(UserId(1)), List(UserId(2)))

class Main:
  def main(args: Array[String]): Unit = {
    
  }