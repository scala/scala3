import scala.quoted._
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

  protected def processCompilationUnit(using QuoteContext)(root: qctx.tasty.Tree): Unit =
    import qctx.tasty._
    inspectClass(root)

  private def inspectClass(using QuoteContext)(tree: qctx.tasty.Tree): Unit =
    import qctx.tasty._
    tree match {
      case t: PackageClause =>
        t.stats.map( m => inspectClass(m) )
      case t: ClassDef =>
        kids = t.symbol.children.map(_.fullName)

      case x =>
    }
