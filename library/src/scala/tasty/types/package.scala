package scala.tasty

package object types {

  object ConstantType {
    type Data = constants.Constant
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyConstantType(arg)

  }

  object SymRef {
    type Data = (statements.Definition, MaybeType /* Type | NoPrefix */)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplySymRef(arg)
  }

  object NameRef {
    type Data = (names.Name, MaybeType /* Type | NoPrefix */)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyNameRef(arg)
  }

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

  object ByNameType {
    type Data = Type
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyByNameType(arg)
  }

  object ParamRef {
    type Data = (LambdaType[_, _], Int)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyParamRef(arg)
  }

  object RecursiveThis {
    type Data = RecursiveType
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyRecursiveThis(arg)
  }

}
