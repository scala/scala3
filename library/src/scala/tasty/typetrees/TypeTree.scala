package scala.tasty
package typetrees

/** Trees denoting types */
trait TypeTree extends Tree {
  def tpe: types.Type
}
