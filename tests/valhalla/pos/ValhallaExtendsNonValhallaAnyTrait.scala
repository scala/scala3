import scala.annotation.valhalla

/**
 * Valhalla classes and traits cannot extend non-Valhalla classes or traits
 */

trait AnyTrait extends Any:
  val a: Int = 2
  def add(x:Int, y:Int): Int


@valhalla
trait TraitExtendsAnyTrait extends Any with AnyTrait: // error
  def addOne(x: Int): Int

@valhalla
class VVC extends AnyVal with AnyTrait: // error
  def add(x:Int, y:Int): Int = x + y
  def addOne(x: Int): Int = x + 1