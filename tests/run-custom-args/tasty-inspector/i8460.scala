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
    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
    val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
    val tastyFiles = allTastyFiles.filter(_.contains("TraitParams"))

    // Tasty Scala Class
    val inspect1 = new TestInspector_Children()
    inspect1.inspectTastyFiles(allTastyFiles.filter(_.contains("Vehicle")))
    assert(inspect1.kids == List("Truck","Car","Plane"))

    // Java Class
    val inspect2 = new TestInspector_Children()
    inspect2.inspectTastyFiles(allTastyFiles.filter(_.contains("Flavor")))
    assert(inspect2.kids == List("Vanilla","Chocolate","Bourbon"))
  }
}

class TestInspector_Children() extends TastyInspector:

  var kids: List[String] = Nil

  protected def processCompilationUnit(using Quotes)(root: quotes.reflect.Tree): Unit =
    import quotes.reflect._
    inspectClass(root)

  private def inspectClass(using Quotes)(tree: quotes.reflect.Tree): Unit =
    import quotes.reflect._
    tree match {
      case t: PackageClause =>
        t.stats.map( m => inspectClass(m) )
      case t: ClassDef =>
        kids = t.symbol.children.map(_.fullName)

      case x =>
    }
