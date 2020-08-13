import scala.tasty.Reflection
import scala.tasty.inspector._

case class I8215(id: String)

object Test {
  def main(args: Array[String]): Unit = {

    // Tasty Scala Class
    val inspect1 = new TestInspector_NonTasty()
    inspect1.inspect("", List("I8215"))
    assert(inspect1.isJava == false)
    assert(inspect1.isScala2 == false)
    assert(inspect1.className == "")

    // Java Class
    val inspect2 = new TestInspector_NonTasty()
    inspect2.inspect("", List("java.util.UUID"))
    assert(inspect2.isJava == true)
    assert(inspect2.isScala2 == false)
    assert(inspect2.className == "java.util.UUID")

    // Legacy non-Tasty Scala class
    val inspect3 = new TestInspector_NonTasty()
    inspect3.inspect("", List("scala.collection.immutable.RedBlackTree"))
    assert(inspect3.isJava == false)
    assert(inspect3.isScala2 == true)
    assert(inspect3.className == "scala.collection.immutable.RedBlackTree")
  }
}

class TestInspector_NonTasty() extends TastyInspector:

  var isJava: Boolean = false
  var isScala2: Boolean = false
  var className: String = ""

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit =
    isJava = reflect.Source.isJavaCompilationUnit
    isScala2 = reflect.Source.isScala2CompilationUnit
    className = reflect.Source.compilationUnitClassname
