import scala.annotation.valhalla

@valhalla
trait ValhallaTrait extends Any:
  def add(x:Int, y:Int): Int

class IdentClass extends ValhallaTrait:
  def add(x:Int, y:Int): Int = x + y

// class Main:
//   def main = {
//     val ic = IdentClass()
//     println(ic.add(2,3))
//   }