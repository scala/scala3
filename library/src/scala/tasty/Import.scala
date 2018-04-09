package scala.tasty

trait Import extends Statement

sealed trait ImportSelector

object ImportSelector {
  final case class Simple(id: Id) extends ImportSelector
  final case class Rename(id1: Id, id2: Id) extends ImportSelector
  final case class Omit(id: Id) extends ImportSelector
}
