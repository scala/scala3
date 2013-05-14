package dotty.tools.dotc
package printing

import core._

import Contexts._, Texts._, Decorators._

trait Showable {

  def toText(implicit ctx: Context): Text

  def show(implicit ctx: Context): String = toText.show

}