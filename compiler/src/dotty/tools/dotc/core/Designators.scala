package dotty.tools
package dotc
package core

import Names._
import NameKinds.SignedName
import Types.{TypeRef, NameSpace, noNameSpace}
import Symbols.Symbol
import Contexts.Context

/** Defines a common superclass of Name and Symbol and its Term/Type variants
 *  Designators are used in reference type to identity what is referred to.
 */
object Designators {

  abstract class Designator extends util.DotClass { self =>

    type ThisName <: Name

    /** Classifiers and casts, to be overridden in implemetations */
    def isName: Boolean = false
    def isSymbol: Boolean = false

    def isTerm(implicit ctx: Context) = false
    def isType(implicit ctx: Context) = false

    def asTerm(implicit ctx: Context): TermDesignator = unsupported("asTerm")
    def asType(implicit ctx: Context): TypeDesignator = unsupported("asType")

    def withNameSpace(space: NameSpace)(implicit ctx: Context): Designator { type ThisName = self.ThisName } =
      if (space == noNameSpace) this
      else localName(this.asInstanceOf[ThisName], space)

    /** Localize this name to the owner of `sym` if `sym` is private */
    def localizeIfPrivate(sym: Symbol)(implicit ctx: Context): Designator { type ThisName = self.ThisName } =
      if (sym.isPrivate) withNameSpace(sym.owner.typeRef) else this

    def withSig(sig: Signature): Designator{ type ThisName = TermName } = {
      val unsigned = this.asInstanceOf[TermName].exclude(SignedName)
      if (sig eq Signature.NotAMethod) unsigned else SignedName(unsigned, sig)
    }
  }

  type TermDesignator = Designator { type ThisName = TermName }
  type TypeDesignator = Designator { type ThisName = TypeName }

  /** Names that come with the namespace where they are defined.
   *  Used to give a stable reference to private names, and also to
   *  Scala 2x inner classes.
   */
  case class LocalName[N <: Name](name: N, nameSpace: TypeRef) extends Designator {
    type ThisName = N

    override def isTerm(implicit ctx: Context) = name.isTermName
    override def isType(implicit ctx: Context) = name.isTypeName

    override def asTerm(implicit ctx: Context): TermDesignator = {
      name.asTermName
      this.asInstanceOf[TermDesignator]
    }
    override def asType(implicit ctx: Context): TypeDesignator = {
      name.asTypeName
      this.asInstanceOf[TypeDesignator]
    }

    override def withNameSpace(space: NameSpace)(implicit ctx: Context) =
      name.withNameSpace(space).asInstanceOf[Designator { type ThisName = N }]

    override def withSig(sig: Signature) =
      LocalName(name.withSig(sig).asInstanceOf[TermName], nameSpace)
  }

  /** Introduced to overcome shortcoming with refined type inference of case classes:
   *  LocalName's result type is always Designator, without a refinement.
   */
  def localName[N <: Name](name: N, nameSpace: TypeRef): Designator { type ThisName = N } =
    LocalName(name, nameSpace).asInstanceOf[Designator { type ThisName = N }]
}