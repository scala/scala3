sealed trait A:
  type Blah <: reflect.Enum

case object B extends A:
  override class Blah extends reflect.Enum: // deprecation
    // Members declared in scala.reflect.Enum
    def ordinal: Int = ???

    // Members declared in scala.Equals
    def canEqual(that: Any): Boolean = ???

    // Members declared in scala.Product
    def productArity: Int = ???
    def productElement(n: Int): Any = ???

case object C extends A:
  override enum Blah: // error
    case Testing

sealed trait Alt:
  type Blah <: reflect.Enum
  val Blah: AnyRef

case object Alternatively extends Alt:
  override enum Blah: // error
    case Testing

trait Blahful:
  type Blah[A, B]

object D extends Blahful:
  override infix enum Blah[A, B]: // error
    case Baz[A, B](a: A, b: B) extends Blah[A, B]
