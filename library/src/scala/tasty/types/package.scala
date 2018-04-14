package scala.tasty

import scala.runtime.tasty.Toolbox
import scala.tasty.trees.Term

package object types {

  object ConstantType {
    type Data = constants.Constant
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyConstantType(arg)

  }

  object SymRef {
    type Data = (scala.tasty.Symbol, MaybeType /* Type | NoPrefix */)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySymRef(arg)
  }

  object NameRef {
    type Data = (names.Name, MaybeType /* Type | NoPrefix */)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyNameRef(arg)
  }

  object SuperType {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySuperType(arg)
  }

  object Refinement {
    type Data = (Type, names.Name, MaybeType /* Type | TypeBounds */)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyRefinement(arg)
  }

  object AppliedType {
    type Data = (Type, List[types.MaybeType /* Type | TypeBounds */])
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAppliedType(arg)
  }

  object AnnotatedType {
    type Data = (Type, Term)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAnnotatedType(arg)
  }

  object AndType {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAndType(arg)
  }

  object OrType {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyOrType(arg)
  }

  object ByNameType {
    type Data = Type
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyByNameType(arg)
  }

  object ParamRef {
    type Data = (LambdaType[_, _], Int)
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyParamRef(arg)
  }

  object RecursiveThis {
    type Data = RecursiveType
    def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyRecursiveThis(arg)
  }

}
