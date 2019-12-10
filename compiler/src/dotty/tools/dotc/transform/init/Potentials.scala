package dotty.tools.dotc
package transform
package init

import ast.tpd._
import core._
import Types._, Symbols._, Contexts._

import Effects._, Summary._

object Potentials {
  type Potentials = Set[Potential]
  val empty: Potentials = Set.empty

  def show(pots: Potentials)(implicit ctx: Context): String =
    pots.map(_.show).mkString(", ")

  sealed trait Potential {
    def size: Int
    def show(implicit ctx: Context): String
    def source: Tree
  }

  case class ThisRef(cls: ClassSymbol)(val source: Tree) extends Potential {
    val size: Int = 1
    def show(implicit ctx: Context): String = cls.name.show + ".this"
  }

  case class SuperRef(cls: ClassSymbol, supercls: ClassSymbol)(val source: Tree) extends Potential {
    val size: Int = 1
    def show(implicit ctx: Context): String = cls.name.show + ".super[" + supercls.name.show + "]"
  }

  /** A warm potential represents an object of which all fields are initialized, but it may contain
   *  reference to objects under initialization.
   *
   *  @param cls The concrete class of the object
   *  @param outers The potentials for the immdiate outer `this`. One entry for each class in the inheritance hierarchy.
   */
  case class Warm(cls: ClassSymbol, outers: Map[ClassSymbol, Potentials])(val source: Tree) extends Potential {
    def size: Int = 1
    def show(implicit ctx: Context): String = "Warm[" + cls.show + "]"
  }

  case class FieldReturn(potential: Potential, field: Symbol)(val source: Tree) extends Potential {
    def size: Int = potential.size
    def show(implicit ctx: Context): String = potential.show + "." + field.name.show
  }

  case class MethodReturn(potential: Potential, symbol: Symbol, virtual: Boolean)(val source: Tree) extends Potential {
    def size: Int = potential.size + 1
    def show(implicit ctx: Context): String = {
      val modifier = if (virtual) "" else "(static)"
      potential.show + "." + symbol.name.show + modifier
    }
  }

  case class Cold(cls: ClassSymbol, definite: Boolean)(val source: Tree) extends Potential {
    def size: Int = 1
    def show(implicit ctx: Context): String = "Cold[" + cls.show + "]"
  }

  case class Fun(potentials: Potentials, effects: Effects)(val source: Tree) extends Potential {
    def size: Int = 1
    def show(implicit ctx: Context): String =
      "Fun[pots = " + potentials.map(_.show).mkString(";") + ", effs = " + effects.map(_.show).mkString(";") + "]"
  }

  // ---------      operations on potentials       ---------

  def (ps: Potentials) select (symbol: Symbol, source: Tree, virtual: Boolean = true, bindings: Map[Symbol, Potentials] = Map.empty)(implicit ctx: Context): Summary =
    ps.foldLeft(Summary.empty) { case ((pots, effs), pot) =>
      if (pot.size > 1)
        (pots, effs + Leak(pot)(source))
      else if (symbol.is(Flags.Method))
          (
            pots + MethodReturn(pot, symbol, virtual)(source),
            effs + MethodCall(pot, symbol, virtual)(source)
          )
      else
        (pots + FieldReturn(pot, symbol)(source), effs + FieldAccess(pot, symbol)(source))
    }

  def (ps: Potentials) leak(source: Tree): Effects = ps.map(Leak(_)(source))
}