package scala.tasty
package modifiers

import scala.runtime.tasty.Toolbox
import scala.tasty.trees.Term

trait Modifier

object Flags {
  type Data = FlagSet
  def unapply(arg: Modifier)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyFlags(arg)
}

object QualifiedPrivate {
  type Data = types.Type
  def unapply(arg: Modifier)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyQualifiedPrivate(arg)
}

object QualifiedProtected {
  type Data = types.Type
  def unapply(arg: Modifier)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyQualifiedProtected(arg)
}

object Annotation {
  type Data = Term
  def unapply(arg: Modifier)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAnnotation(arg)
}
