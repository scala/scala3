package scala.tasty

import scala.tasty.term.Term

package object typetree {

  object Synthetic {
    def unapply(arg: TypeTree)(implicit ext: Extractor): Boolean = ext.unapplySynthetic(arg)
  }

  object Ident {
    type Data = TypeName
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyIdent(arg)
  }

  object Select {
    type Data = (Term, TypeName)
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplySelect(arg)
  }

  object Singleton {
    type Data = Term
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplySingleton(arg)
  }

  //  object Refined {
  //    type Data = (TypeTree, List[Definition])
  //    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyRefined(arg)
  //  }

  object Applied {
    type Data = (TypeTree, List[TypeTree])
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyApplied(arg)
  }

  object TypeBounds {
    type Data = (TypeTree, TypeTree)
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeBounds(arg)
  }

  object Annotated {
    type Data = (TypeTree, Term)
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyAnnotated(arg)
  }

  object And {
    type Data = (TypeTree, TypeTree)
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyAnd(arg)
  }

  object Or {
    type Data = (TypeTree, TypeTree)
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyOr(arg)
  }

  object ByName {
    type Data = TypeTree
    def unapply(arg: TypeTree)(implicit ext: Extractor): Option[Data] = ext.unapplyByName(arg)
  }


}
