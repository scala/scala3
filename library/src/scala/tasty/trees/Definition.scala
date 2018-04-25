package scala.tasty
package trees

trait Definition extends Statement {
  def owner(implicit ctx: Context): Definition
  def localContext(implicit ctx: Context): Context
}
