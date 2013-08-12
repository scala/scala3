package dotty.tools
package dotc

import core._
import Contexts._

class Compiler {

  def newRun(implicit ctx: Context): Run = new Run(this)

}