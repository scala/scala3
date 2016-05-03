package dotty.tools.dottydoc
package model

object pickling {
  import internal._
  import prickle._

  implicit val entityPickler: PicklerPair[Entity] = CompositePickler[Entity]
    .concreteType[NonEntity.type]
    .concreteType[ValImpl]
    .concreteType[DefImpl]
    .concreteType[ClassImpl]
    .concreteType[CaseClassImpl]
    .concreteType[ObjectImpl]
    .concreteType[TraitImpl]
    .concreteType[PackageImpl]

  implicit val packagePickler: PicklerPair[Package] = CompositePickler[Package]
    .concreteType[PackageImpl]
}
