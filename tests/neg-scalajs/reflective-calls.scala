import scala.reflect.{ClassTag, Selectable as ReflectSel}
import ReflectSel.reflectiveSelectable

object Test {
  /* Make sure that an explicit desugaring of the legit cases actually compiles,
   * ensuring that the error cases we test are actually testing the right things.
   */
  def sanityCheck(): Unit = {
    val receiver: ReflectSel = ???
    receiver.selectDynamic("foo") // OK
    receiver.applyDynamic("foo")() // OK
    receiver.applyDynamic("foo", classOf[String], classOf[List[_]])("bar", Nil) // OK
  }

  def nonLiteralMethodName(): Unit = {
    val receiver: ReflectSel = ???
    val methodName: String = "foo"
    receiver.selectDynamic(methodName) // error
    receiver.applyDynamic(methodName)() // error
  }

  def nonLiteralClassOf(): Unit = {
    val receiver: ReflectSel = ???
    val myClassOf: Class[String] = classOf[String]
    receiver.applyDynamic("foo", myClassOf, classOf[List[_]])("bar", Nil) // error
  }

  def classOfVarArgs(): Unit = {
    val receiver: ReflectSel = ???
    val classOfs: List[Class[_]] = List(classOf[String], classOf[List[_]])
    receiver.applyDynamic("foo", classOfs*)("bar", Nil) // error
  }

  def argsVarArgs(): Unit = {
    val receiver: ReflectSel = ???
    val args: List[Any] = List("bar", Nil)
    receiver.applyDynamic("foo", classOf[String], classOf[List[_]])(args*) // error
  }
}
