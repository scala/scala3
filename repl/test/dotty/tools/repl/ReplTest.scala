package dotty.tools
package repl

import org.junit.Before

class ReplTest extends Repl(Array(
  // TODO: get rid of this!
  "-classpath",
    List("../out/bootstrap/dotty-library-bootstrapped/scala-0.2/classes",
         "../interfaces/target/classes").mkString(":")
)) {

  /** Make sure the context is new before each test */
  @Before def init = {
    myCtx = initializeCtx
  }
}

