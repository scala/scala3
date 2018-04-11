package scala.tasty

/** Trees denoting types */
trait TypeTree extends Positioned {
  def tpe: Type
}
