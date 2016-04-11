package dotty.tools.dottydoc
package model
package comment

trait CommentCooker {
  trait Context
  def cook(comment: String)(implicit ctx: Context): String = ""
}
