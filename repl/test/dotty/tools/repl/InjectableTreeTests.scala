package dotty.tools
package repl

import org.junit.Assert._
import org.junit.Test

class ReplTest extends Repl(Array(
  // TODO: get rid of this!
  "-classpath",
    List("../out/bootstrap/dotty-library-bootstrapped/scala-0.2/classes",
         "../interfaces/target/classes").mkString(":")
))


class InjectableTreeTests extends ReplTest {
  @Test def crashCheck = {
    implicit val ctx = myCtx
    val injTree = InjectableTree()
    val parsed @ Parsed(_,_) = ParseResult("def foo: 1 = 1")
    val symName = injTree.obj.trees(1).symbol.show
    assert(symName == "object ReplSession", symName)
  }
}
