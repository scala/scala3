sealed trait T
trait Trait extends T
class Clazz extends T
object Obj extends T

trait OpenTrait
trait OpenTrait2
class OpenClass

trait Unrelated

object Test {
  type Alias1 = OpenTrait
  def traitAndClass(s: T & Alias1) = s match {
    case _: Unrelated => ;
  }

  type Alias2 = OpenTrait & OpenClass
  def classOnly(s: T & Alias2) = s match {
    case _: Unrelated => ;
  }

  def classOnlyRefined(s: T & (Alias2 & OpenTrait2) { val x: Int }) = s match {
    case _: Unrelated => ;
  }
}
