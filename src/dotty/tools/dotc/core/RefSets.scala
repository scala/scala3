package dotty.tools.dotc
package core

import Symbols._, Flags._, Types._, Contexts._

object RefSets {

  trait RefSet {
    def isEmpty: Boolean
    def containsSig(sig: Signature)(implicit ctx: Context): Boolean
    def filter(p: Symbol => Boolean)(implicit ctx: Context): RefSet
    def filterDisjoint(syms: RefSet)(implicit ctx: Context): RefSet
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): RefSet
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): RefSet
    def seenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): RefSet
    def union(that: RefSet) =
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else RefUnion(this, that)
  }

  case class RefUnion(syms1: RefSet, syms2: RefSet) extends RefSet {
    assert(!syms1.isEmpty && !syms2.isEmpty)
    private def derivedUnion(s1: RefSet, s2: RefSet) =
      if (s1.isEmpty) s2
      else if (s2.isEmpty) s1
      else if ((s1 eq syms2) && (s2 eq syms2)) this
      else new RefUnion(s1, s2)
    def isEmpty = false
    def containsSig(sig: Signature)(implicit ctx: Context) =
      (syms1 containsSig sig) || (syms2 containsSig sig)
    def filter(p: Symbol => Boolean)(implicit ctx: Context) =
      derivedUnion(syms1 filter p, syms2 filter p)
    def filterDisjoint(syms: RefSet)(implicit ctx: Context): RefSet =
      derivedUnion(syms1 filterDisjoint syms, syms2 filterDisjoint syms)
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): RefSet =
      derivedUnion(syms1 filterExcluded flags, syms2 filterExcluded flags)
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): RefSet =
      derivedUnion(syms1 filterAccessibleFrom pre, syms2 filterAccessibleFrom pre)
    def seenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): RefSet =
      derivedUnion(syms1.seenFrom(pre, owner), syms2.seenFrom(pre, owner))
  }

  trait RefSetSingleton extends RefSet { this: SymRef =>
    def isEmpty = isWrong
    def containsSig(sig: Signature)(implicit ctx: Context) =
      signature == sig
    def filter(p: Symbol => Boolean)(implicit ctx: Context): RefSet =
      if (p(symbol)) this else NoType
    def filterDisjoint(syms: RefSet)(implicit ctx: Context): RefSet =
      if (syms.containsSig(signature)) NoType else this
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): RefSet =
      if (symbol.hasFlag(flags)) NoType else this
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): RefSet =
      if (symbol.isAccessibleFrom(pre)) this else NoType
    def seenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): RefSet =
      asSeenFrom(pre, owner).asInstanceOf[RefSet]
  }
}