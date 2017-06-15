package dotty.tools
package repl

class ReplTest extends Repl(Array(
  // TODO: get rid of this!
  "-classpath",
    List("../out/bootstrap/dotty-library-bootstrapped/scala-0.2/classes",
         "../interfaces/target/classes").mkString(":")
))

