import scala.quoted._
import scala.tasty.inspector._

@main def Test = {
  // in dotty-example-project
  val inspector = new TastyInspector {
    protected def processCompilationUnit(using QuoteContext)(tree: qctx.reflect.Tree): Unit = {
      println(tree.show)
    }
  }
  inspector.inspect("", List("TraitParams"))
}

object TraitParams {

  trait Base(val msg: String)
  class A extends Base("Hello")
  class B extends Base("Dotty!")

  // Union types only exist in Dotty, so there's no chance that this will accidentally be compiled with Scala 2
  private def printMessages(msgs: (A | B)*) = println(msgs.map(_.msg).mkString(" "))

  def test: Unit = {

    printMessages(new A, new B)

    // Sanity check the classpath: this won't run if the dotty jar is not present.
    val x: Int => Int = z => z
    x(1)
  }
}
