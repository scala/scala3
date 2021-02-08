
import scala.quoted.*
import scala.quoted.staging.*

object Test {

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    '{
      def m(using Quotes) = '{
        errorOnceInlined() // Should not be inlined while `run` is executed. It would be inlined in the next stage.
      }
      ()
    }
  }

  inline def errorOnceInlined() = compiletime.error("Error")

}
