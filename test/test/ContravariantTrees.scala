package test

import language.higherKinds
import dotty.tools.dotc.core._
import Flags._, Names._, StdNames._
import annotation.unchecked.uncheckedVariance

object ContravariantTrees {

  type Untyped = Null

  case class Modifiers[-T >: Untyped] (
    flags: FlagSet = EmptyFlags,
    privateWithin: TypeName = tpnme.EMPTY,
    annotations: List[Tree[T]] = Nil) {

    def is(fs: FlagSet): Boolean = flags is fs
    def is(fc: FlagConjunction): Boolean = flags is fc

    def | (fs: FlagSet): Modifiers[T] = withFlags(flags | fs)
    def & (fs: FlagSet): Modifiers[T] = withFlags(flags & fs)
    def &~(fs: FlagSet): Modifiers[T] = withFlags(flags &~ fs)

    def toTypeFlags: Modifiers[T] = withFlags(flags.toTypeFlags)
    def toTermFlags: Modifiers[T] = withFlags(flags.toTermFlags)

    private def withFlags(flags: FlagSet) =
      if (this.flags == flags) this
      else copy(flags = flags)

    def withPrivateWithin(pw: TypeName) =
      if (pw.isEmpty) this
      else copy(privateWithin = pw)

    def hasFlags = flags != EmptyFlags
    def hasAnnotations = annotations.nonEmpty
    def hasPrivateWithin = privateWithin != tpnme.EMPTY
  }

  abstract class Tree[-T >: Untyped] {
    private[this] var myTpe: T = _
    private def setMyTpe(tpe: T) = myTpe = tpe

    def tpe: T @uncheckedVariance = {
      if (myTpe == null) throw new Error()
      myTpe
    }
  }

  trait TermTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[T >: Untyped] <: TermTree[T]
  }

  case class Select[-T >: Untyped](qualifier: Tree[T], name: Name)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Select[T]
  }

    /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[-T >: Untyped](mods: Modifiers[T], name: TermName, tparams: List[Tree[T]], vparamss: List[List[Tree[T]]], tpt: Tree[T], rhs: Tree[T])
    extends Tree[T] {
    type ThisTree[T >: Untyped] = DefDef[T]
  }

}
