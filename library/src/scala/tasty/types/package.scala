package scala.tasty

package object types {

  object ConstantType {
    type Data = constants.Constant
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyConstantType(arg)

  }

  //  case class SymRef(sym: Definition, qualifier: Type | NoPrefix = NoPrefix) extends Type
  //  case class NameRef(name: Name, qualifier: Type | NoPrefix = NoPrefix) extends Type // NoPrefix means: select from _root_

  object SuperType {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplySuperType(arg)
  }

  object Refinement {
    type Data = (Type, names.Name, Type)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyRefinement(arg)
  }

  object AppliedType {
    type Data = (Type, List[types.MaybeType /* Type | TypeBounds */])
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyAppliedType(arg)
  }

  object AnnotatedType {
    type Data = (Type, terms.Term)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyAnnotatedType(arg)
  }

  object AndType {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyAndType(arg)
  }

  object OrType {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyOrType(arg)
  }


  object TypeBounds {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeBounds(arg)
  }

}
