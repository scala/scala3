import scala.annotation.unchecked.uncheckedVariance

object Test {
  def to[Col[_]](factory: collection.Factory[Int, Col[Int @uncheckedVariance]]) = ???

  val test = to(List)
}
