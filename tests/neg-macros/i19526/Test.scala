package crash.test

case class Stack private[crash] (
    exports: String,
    dependsOn: Vector[Int]
)

trait StackFactory:
  val exports: Export.type = Export

  def apply(dependsOn: Int*): Stack =
    Export().copy(dependsOn = dependsOn.toVector)

// nopos-error
