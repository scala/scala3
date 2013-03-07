package dotty.tools.dotc.core

import Contexts._

trait Showable {

  def show(implicit ctx: Context): String

}