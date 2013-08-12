package dotty.tools
package dotc

import core._
import Contexts._

class Run(comp: Compiler)(implicit ctx: Context) {

  def compile(fileNames: List[String]): Unit =
    for (name <- fileNames) println(s"<compiling $name>")

}