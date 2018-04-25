package scala.tasty

import scala.tasty.trees.Term

package object types {

  object ConstantType {
    type Data = constants.Constant
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyConstantType(arg)
  }

  object SymRef {
    type Data = (trees.Definition, MaybeType /* Type | NoPrefix */)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySymRef(arg)
  }

  object NameRef {
    type Data = (names.Name, MaybeType /* Type | NoPrefix */)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyNameRef(arg)
  }

  object SuperType {
    type Data = (Type, Type)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySuperType(arg)
  }

  object Refinement {
    type Data = (Type, names.Name, MaybeType /* Type | TypeBounds */)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyRefinement(arg)
  }

  object AppliedType {
    type Data = (Type, List[types.MaybeType /* Type | TypeBounds */])
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyAppliedType(arg)
  }

  object AnnotatedType {
    type Data = (Type, Term)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyAnnotatedType(arg)
  }

  object AndType {
    type Data = (Type, Type)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyAndType(arg)
  }

  object OrType {
    type Data = (Type, Type)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyOrType(arg)
  }

  object ByNameType {
    type Data = Type
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyByNameType(arg)
  }

  object ParamRef {
    type Data = (LambdaType[_, _], Int)
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyParamRef(arg)
  }

  object ThisType {
    type Data = Type
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyThisType(arg)
  }

  object RecursiveThis {
    type Data = RecursiveType
    def unapply(arg: Type)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyRecursiveThis(arg)
  }

}
