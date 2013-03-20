package dotty.tools.dotc
package core

import Contexts._, util.Texts._, Decorators._

trait Showable {

  def toText(implicit ctx: Context): Text

  def show(implicit ctx: Context): String = toText.show

}