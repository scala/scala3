package scala.tasty
package statements

import scala.tasty.names.Name

trait Definition extends Statement {
  def name: Name
  def owner: Definition
}
