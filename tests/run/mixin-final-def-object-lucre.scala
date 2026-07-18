trait EventLike

trait GrandParent:
  def changed: EventLike

trait HasChanged extends GrandParent:
  override def changed: EventLike

abstract class Parent extends GrandParent:
  object changed extends EventLike

class Child extends Parent with HasChanged

object Test:
  def main(args: Array[String]): Unit =
    val child = Child()
    println(child.changed)
    println((child: HasChanged).changed)
end Test
