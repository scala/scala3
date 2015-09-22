import scala.util.Random
import scala.collection.immutable.List._
object Test {
  def test = {
    val rand = new Random
    rand.shuffle(List(1,2))//(List.canBuildFrom[Int]) // fails
  }
}
