package dotty.tools.dottydoc
package model

object pickling {
  import internal._
  import prickle._
  import comment._

  implicit val inlinePickler: PicklerPair[Inline] = CompositePickler[Inline]
    .concreteType[Chain]
    .concreteType[Italic]
    .concreteType[Bold]
    .concreteType[Underline]
    .concreteType[Superscript]
    .concreteType[Subscript]
    .concreteType[Link]
    .concreteType[Monospace]
    .concreteType[Text]

  implicit val entityLinkPicker: PicklerPair[MaterializableLink] = CompositePickler[MaterializableLink]
    .concreteType[UnsetLink]
    .concreteType[MaterializedLink]

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
