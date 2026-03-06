import scala.annotation.valhalla

/**
 * Valhalla Class Extends AnyRef Class
 * */
abstract class Abs {
  def addOne(x : Int) : Int
}

@valhalla
class ValhallaAnnotWithoutExtendingAnyVal extends Abs{ // error
  def addOne(x : Int) : Int = x + 1
}

/**
 * Valhalla Trait Extends AnyRef 2
 * */
@valhalla
trait Uni extends Any

@valhalla
trait AnyRefTrait extends Uni // error

trait Trait:
  def add(x:Int, y:Int): Int

@valhalla
class VVC extends AnyVal with Trait: // error
  def add(x:Int, y:Int): Int = x + y