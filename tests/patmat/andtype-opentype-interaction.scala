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
abstract class OpenAbstractClass

trait Unrelated

object Test {
  def m1(s: T & OpenTrait) = s match {
    case _: Unrelated => ;
  }

  def m2(s: T & OpenClass) = s match {
    case _: Unrelated => ;
  }

  def m3(s: T & OpenAbstractClass) = s match {
    case _: Unrelated => ;
  }
}
