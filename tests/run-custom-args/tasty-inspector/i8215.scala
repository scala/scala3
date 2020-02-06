import scala.tasty.Reflection
import scala.tasty.inspector._

case class I8215(id: String)

object Test {
  def main(args: Array[String]): Unit = {
  	val inspect1 = new TestInspector()
  	inspect1.inspect("", List("I8215"))
  	assert(inspect1.gotJava == None)

  	val inspect2 = new TestInspector()
  	inspect2.inspect("", List("java.util.UUID"))
  	assert(inspect2.gotJava == Some("java.util.UUID"))
  }
}

class TestInspector() extends TastyInspector

  var gotJava: Option[String] = None

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{given,_}
    gotJava = reflect.rootContext.javaCompilationUnitClassname()