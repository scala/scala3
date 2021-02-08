
import scala.quoted.*
import scala.quoted.staging.*

object Test {

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    '{
      given Int = 10
      compiletime.summonInline[Int]
      ()
    }
  }
}
