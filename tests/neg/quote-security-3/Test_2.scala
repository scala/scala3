import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    Macros.foo() // error: Failed to evaluate inlined quote. Caused by: access denied ("java.util.PropertyPermission" "user.dir" "read")
  }
}
