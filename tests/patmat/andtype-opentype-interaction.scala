sealed trait T
trait Trait extends T
class Clazz extends T
abstract class AbstractClass extends T
// none of the below can be extended
sealed trait SealedTrait extends T
final class FinalClass extends T
sealed class SealedClass extends T
sealed abstract class SealedAbstractClass extends T
object Obj extends T

trait OpenTrait
class OpenClass
class OpenClassSubclass extends OpenClass
abstract class OpenAbstractClass

trait Unrelated

trait OpenTrait2
class OpenClass2

object Test {
  def m1a(s: T & OpenTrait) = s match {
    case _: Unrelated => ;
  }

  def m1b(s: T & OpenTrait & OpenTrait2) = s match {
    case _: Unrelated => ;
  }

  def m2(s: T & OpenClass) = s match {
    case _: Unrelated => ;
  }

  def m2b(s: T & OpenTrait & OpenClass) = s match {
    case _: Unrelated => ;
  }

  def m2c(s: (T & OpenClass) & (OpenTrait & OpenClass2)) = s match {
    case _: Unrelated => ;  // OK since scrutinee is the empty type
  }

  def m3(s: T & OpenAbstractClass) = s match {
    case _: Unrelated => ;
  }

  def m4(s: (T & OpenClass) & (OpenTrait & OpenClassSubclass)) = s match {
    case _: Unrelated => ;
  }
}
