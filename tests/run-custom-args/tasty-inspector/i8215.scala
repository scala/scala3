import scala.tasty.Reflection
import scala.tasty.inspector._

case class I8215(id: String)

object Test {
  def main(args: Array[String]): Unit = {

    // Tasty Scala Class
    val inspect1 = new TestInspector()
    inspect1.inspect("", List("I8215"))
    assert(inspect1.gotJava == None)
    assert(inspect1.gotNonTastyScala == None)

    // Java Class
    val inspect2 = new TestInspector()
    inspect2.inspect("", List("java.util.UUID"))
    assert(inspect2.gotJava == Some("java.util.UUID"))
    assert(inspect2.gotNonTastyScala == None)

    // Legacy non-Tasty Scala class
    val inspect3 = new TestInspector()
    inspect3.inspect("", List("scala.collection.immutable.RedBlackTree"))
    assert(inspect3.gotJava == None)
    assert(inspect3.gotNonTastyScala == Some("scala.collection.immutable.RedBlackTree"))    
  }
}

class TestInspector() extends TastyInspector

  var gotJava: Option[String] = None
  var gotNonTastyScala: Option[String] = None

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{given,_}
    gotJava = reflect.rootContext.javaCompilationUnitClassname()
    gotNonTastyScala = reflect.rootContext.nonTastyScalaCompilationUnitClassname()