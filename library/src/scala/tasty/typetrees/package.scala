package scala.tasty

import scala.runtime.tasty.Toolbox

package object typetrees {

  object Synthetic {
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Boolean = toolbox.unapplySynthetic(arg)
  }

  object Ident {
    type Data = names.TypeName
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyIdent(arg)
  }

  object Select {
    type Data = (terms.Term, names.TypeName)
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySelect(arg)
  }

  object Singleton {
    type Data = terms.Term
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySingleton(arg)
  }

  object Refined {
    type Data = (TypeTree, List[statements.Definition])
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyRefined(arg)
  }

  object Applied {
    type Data = (TypeTree, List[TypeTree])
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyApplied(arg)
  }

  object Annotated {
    type Data = (TypeTree, terms.Term)
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAnnotated(arg)
  }

  object And {
    type Data = (TypeTree, TypeTree)
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAnd(arg)
  }

  object Or {
    type Data = (TypeTree, TypeTree)
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyOr(arg)
  }

  object ByName {
    type Data = TypeTree
    def unapply(arg: TypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyByName(arg)
  }


}
