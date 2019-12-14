package dotty.tools.dotc
package transform
package init

import scala.collection.mutable

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

  sealed trait RootPotential extends Potential {
    def classSymbol: ClassSymbol
  }

  case class ThisRef(classSymbol: ClassSymbol)(val source: Tree) extends RootPotential {
    val size: Int = 1
    def show(implicit ctx: Context): String = classSymbol.name.show + ".this"
  }

  case class SuperRef(root: RootPotential, supercls: ClassSymbol)(val source: Tree) extends Potential {
    val size: Int = 1
    def show(implicit ctx: Context): String = root.show + ".super[" + supercls.name.show + "]"
  }

  /** A warm potential represents an object of which all fields are initialized, but it may contain
   *  reference to objects under initialization.
   *
   *  @param classSymbol  The concrete class of the object
   *  @param outer        The potentials for the immdiate outer `this`
   */
  case class Warm(classSymbol: ClassSymbol, outer: Potentials)(val source: Tree) extends RootPotential {
    assert(outer.size <= 1, "outer size > 1, outer = " + outer)

    def size: Int = 1
    def show(implicit ctx: Context): String = "Warm[" + cls.show + "]"

    private val outerCache: mutable.Map[ClassSymbol, Potentials] = mutable.Map.empty
    def outerFor(cls: ClassSymbol)(implicit env: Env): Potentials =
      if (outerCache.contains(cls)) outerCache(cls)
      else if (cls `eq` classSymbol) outer
      else {
        val bottomClsSummary = env.summaryOf(classSymbol)
        val objPart = ObjectPart(this, classSymbol, outer, bottomClsSummary.parentOuter)
        val pots = objPart.outerFor(cls)
        outerCache(cls) = pots
        pots
      }
  }

  /** The Outer potential for `classSymbol` of the object `pot`
   *
   *  It's only used internally for expansion of potentials.
   *
   *  Note: Usage of `Type.baseType(cls)` may simplify the code.
   *        Current implementation avoids using complex type machinary,
   *        and may be potentially faster.
   */
  case class Outer(pot: Potential, classSymbol: ClassSymbol)(val source: Tree) extends Potential {
    def size: Int = 1
    def show(implicit ctx: Context): String = "Outer[" + pot.show + ", " + classSymbol.show + "]"
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

  case class Cold()(val source: Tree) extends Potential {
    def size: Int = 1
    def show(implicit ctx: Context): String = "Cold"
  }

  case class Fun(potentials: Potentials, effects: Effects)(val source: Tree) extends Potential {
    def size: Int = 1
    def show(implicit ctx: Context): String =
      "Fun[pots = " + potentials.map(_.show).mkString(";") + ", effs = " + effects.map(_.show).mkString(";") + "]"
  }

  // ------------------ operations on potentials ------------------

  def (ps: Potentials) select (symbol: Symbol, source: Tree, virtual: Boolean = true)(implicit ctx: Context): Summary =
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

  def asSeenFrom(pot: Potential, thisValue: Potential, currentClass: ClassSymbol, outer: Potentials)(implicit env: Env): Potentials =
    pot match {
      case MethodReturn(pot1, sym, virtual) =>
        val pots = asSeenFrom(pot1, thisValue, currentClass, outer)
        pots.map { MethodReturn(_, sym, virtual)(pot.source) }

      case FieldReturn(pot1, sym) =>
        val pots = asSeenFrom(pot1, thisValue, currentClass, outer)
        pots.map { FieldReturn(_, sym)(pot.source) }

      case Outer(pot1, cls) =>
        val pots = asSeenFrom(pot1, thisValue, currentClass, outer)
        pots map { Outer(_, cls)(pot.source) }

      case ThisRef(cls) =>
        if (cls `eq` currentClass) Potentials.empty + thisValue
        else if (cls `eq` currentClass.owner) outer
        else ???

      case Fun(pots, effs) =>
        val pots1 = asSeenFrom(pots, thisValue, currentClass, outer)
        val effs1 = Effects.asSeenFrom(effs, thisValue, currentClass, outer)
        Fun(pots1, effs1)(pot.source)

      case Warm(cls, outer2) =>
        // widening to terminate
        val thisValue2 = thisValue match {
          case Warm(cls, outer) => Warm(cls, Cold()(outer.source))(thisValue.source)
          case _                => thisValue
        }

        val outer3 = asSeenFrom(outer2, thisValue2, currentClass, outer)
        Warm(cls, outer3)(pot.source)

      case _: Cold =>
        Potentials.empty + pot

      case SuperRef(root, supercls) =>
        val pots = asSeenFrom(root, thisValue, currentClass, outer)
        pots.map { SuperRef(_, supercls)(pot.source) }
    }

  def asSeenFrom(pots: Potentials, thisValue: RootPotential, currentClass: ClassSymbol, outer: Potentials)(implicit env: Env): Potentials =
    pots.flatMap(asSeenFrom(_, thisValue, currentClass, outer))
}