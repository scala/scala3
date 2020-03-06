import scala.tasty.Reflection
import scala.tasty.inspector._

// Ambiguous member names
sealed trait Vehicle
case class Truck(numberOfWheels: Int) extends Vehicle
case class Car(numberOfWheels: Int, color: String) extends Vehicle
case class Plane(numberOfEngines: Int) extends Vehicle

// Case object implementation
sealed trait Flavor
case object Vanilla extends Flavor
case object Chocolate extends Flavor
case object Bourbon extends Flavor

object Test {
  def main(args: Array[String]): Unit = {

    // Tasty Scala Class
    val inspect1 = new TestInspector_Children()
    inspect1.inspect("", List("Vehicle"))
    assert(inspect1.kids == List("Truck","Car","Plane"))

    // Java Class
    val inspect2 = new TestInspector_Children()
    inspect2.inspect("", List("Flavor"))
    assert(inspect2.kids == List("Vanilla","Chocolate","Bourbon"))
  }
}

class TestInspector_Children() extends TastyInspector:

  var kids: List[String] = Nil

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit =
    import reflect._
    inspectClass(reflect)(root)

  private def inspectClass(reflect: Reflection)(tree: reflect.Tree): Unit =
    import reflect.{given _, _}
    tree match {
      case t: reflect.PackageClause =>
        t.stats.map( m => inspectClass(reflect)(m) )
      case t: reflect.ClassDef =>
        kids = t.symbol.children.map(_.fullName)

      case x =>
    }
