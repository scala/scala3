import scala.reflect.{ClassTag, Selectable => ReflectSel}
import ReflectSel.reflectiveSelectable

object Test {
  /* Make sure that an explicit desugaring of the legit cases actually compiles,
   * ensuring that the error cases we test are actually testing the right things.
   */
  def sanityCheck(): Unit = {
    val receiver: Any = ???
    reflectiveSelectable(receiver).selectDynamic("foo") // OK
    reflectiveSelectable(receiver).applyDynamic("foo")() // OK
    reflectiveSelectable(receiver).applyDynamic("foo", ClassTag(classOf[String]), ClassTag(classOf[List[_]]))("bar", Nil) // OK
  }

  def badReceider(): Unit = {
    val receiver: ReflectSel = ???
    receiver.selectDynamic("foo") // error
    receiver.applyDynamic("foo")() // error
  }

  def nonLiteralMethodName(): Unit = {
    val receiver: Any = ???
    val methodName: String = "foo"
    reflectiveSelectable(receiver).selectDynamic(methodName) // error
    reflectiveSelectable(receiver).applyDynamic(methodName)() // error
  }

  def nonLiteralClassTag(): Unit = {
    val receiver: Any = ???
    val myClassTag: ClassTag[String] = ClassTag(classOf[String])
    reflectiveSelectable(receiver).applyDynamic("foo", myClassTag, ClassTag(classOf[List[_]]))("bar", Nil) // error
  }

  def classTagVarArgs(): Unit = {
    val receiver: Any = ???
    val classTags: List[ClassTag[_]] = List(ClassTag(classOf[String]), ClassTag(classOf[List[_]]))
    reflectiveSelectable(receiver).applyDynamic("foo", classTags: _*)("bar", Nil) // error
  }

  def argsVarArgs(): Unit = {
    val receiver: Any = ???
    val args: List[Any] = List("bar", Nil)
    reflectiveSelectable(receiver).applyDynamic("foo", ClassTag(classOf[String]), ClassTag(classOf[List[_]]))(args: _*) // error
  }
}
