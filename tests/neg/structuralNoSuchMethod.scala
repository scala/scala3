import scala.reflect.Selectable.reflectiveSelectable

/** Demonstrates limitation of structural method dispatch (in Scala 2.x and dotty).
 *  The method must be defined at exactly the argument types given in the structural type;
 *  Generic instantiation is not possible.
 */
object Test {
  type T = { def f(x: String, y: String): String }

  class C[X] {
    def f(x: X, y: String): String = "f1"
  }

  val x: T = new C[String] // error

  def main(args: Array[String]) =
    try println(x.f("", ""))  // used to throw NoSuchMethodException
    catch {
      case ex: NoSuchMethodException =>
        println("no such method")
    }

}
