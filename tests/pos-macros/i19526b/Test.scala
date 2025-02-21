package crash.test

case class Stack private[crash] (
    exports: String,
    dependsOn: Vector[Int]
)

object Stack:
  @annotation.publicInBinary
  private[crash] def apply(exports: String, dependsOn: Vector[Int]): Stack = new Stack(exports, dependsOn)

trait StackFactory:
  val exports: Export.type = Export

  def apply(dependsOn: Int*): Stack =
    Export().copy(dependsOn = dependsOn.toVector)
