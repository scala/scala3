import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def u(using s: Scope): s.Expr[Unit] = '{}
    println(usingNewScope(u.show))
    println(run(u))
  }
}
