import scala.quoted.*
import scala.quoted.staging.*
import scala.util.control.NonLocalReturns.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit =
    try run {returning('{ { (x: Int) => ${ throwReturn('x) }} apply 0 })}
    catch {
      case ex: dotty.tools.dotc.reporting.Diagnostic.Error =>
        assert(ex.getMessage == "While expanding a macro, a reference to parameter x was used outside the scope where it was defined", ex.getMessage)
    }
}
