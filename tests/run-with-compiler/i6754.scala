
import scala.quoted._

object Test {
  implicit val tbx: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    def y given QuoteContext: Expr[Unit] = '{
      def x given QuoteContext: Expr[Unit] = '{println("bar")}
      println("foo")
      run(x)
    }
    try {
      run(y)
      throw new Exception
    } catch {
      case ex: java.lang.reflect.InvocationTargetException =>
        assert(ex.getTargetException.isInstanceOf[scala.quoted.Toolbox.RunScopeException])
    }
  }
}
