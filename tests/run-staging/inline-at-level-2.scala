
import scala.quoted._
import scala.quoted.staging._

object Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

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
