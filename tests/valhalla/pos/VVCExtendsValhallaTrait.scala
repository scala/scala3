import scala.annotation.valhalla

@valhalla
trait ValhallaTrait extends Any:
  def add(x:Int, y:Int): Int

@valhalla
class VVC extends AnyVal with ValhallaTrait:
  def add(x:Int, y:Int): Int = x + y