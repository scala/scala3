import scala.annotation.valhalla

trait AnyTrait extends Any:
  val a: Int = 2
  def add(x:Int, y:Int): Int

@valhalla
trait TraitExtendsAnyTrait extends Any with AnyTrait:
  def addOne(x: Int): Int

@valhalla
class VVC extends AnyVal with AnyTrait:
  def add(x:Int, y:Int): Int = x + y
  def addOne(x: Int): Int = x + 1