package dotty.tools.dotc
package transform
package init

import scala.collection.mutable

import ast.tpd._
import reporting.trace
import config.Printers.init

import core._
import Types._, Symbols._, Contexts._

import Effects._, Summary._

object Potentials {
  type Potentials = Set[Potential]
  val empty: Potentials = Set.empty

  def show(pots: Potentials)(using Context): String =
    pots.map(_.show).mkString(", ")

  /** A potential represents an aliasing of a value that is possibly under initialization */
  sealed trait Potential {
    def size: Int
    def show(using Context): String
    def source: Tree
  }

  /** The object pointed by `C.this` */
  case class ThisRef(classSymbol: ClassSymbol)(val source: Tree) extends Potential {
    val size: Int = 1
    def show(using Context): String = classSymbol.name.show + ".this"

    /** Effects of a method call or a lazy val access
     *
     *  It assumes all the outer `this` are fully initialized.
     */
    def effectsOf(sym: Symbol)(implicit env: Env): Effects = trace("effects of " + sym.show, init, r => Effects.show(r.asInstanceOf)) {
      val cls = sym.owner.asClass
      val effs = env.summaryOf(cls).effectsOf(sym)
      Effects.asSeenFrom(effs, this, cls, Potentials.empty)
    }

    /** Potentials of a field, a method call or a lazy val access
     *
     */
    def potentialsOf(sym: Symbol)(implicit env: Env): Potentials = trace("potentials of " + sym.show, init, r => Potentials.show(r.asInstanceOf)) {
      val cls = sym.owner.asClass
      val pots = env.summaryOf(cls).potentialsOf(sym)
      Potentials.asSeenFrom(pots, this, cls, Potentials.empty)
    }
  }

  /** The object pointed by `C.super.this`, mainly used for override resolution */
  case class SuperRef(pot: Potential, supercls: ClassSymbol)(val source: Tree) extends Potential {
    val size: Int = 1
    def show(using Context): String = pot.show + ".super[" + supercls.name.show + "]"
  }

  /** A warm potential represents an object of which all fields are initialized, but it may contain
   *  reference to objects under initialization.
   *
   *  @param classSymbol  The concrete class of the object
   *  @param outer        The potential for `this` of the enclosing class
   */
  case class Warm(classSymbol: ClassSymbol, outer: Potential)(val source: Tree) extends Potential {
    def size: Int = 1
    def show(using Context): String = "Warm[" + classSymbol.show + ", outer = " + outer.show + "]"

    /** Effects of a method call or a lazy val access
     *
     *  The method performs prefix and outer substitution
     */
    def effectsOf(sym: Symbol)(implicit env: Env): Effects = trace("effects of " + sym.show, init, r => Effects.show(r.asInstanceOf)) {
      val cls = sym.owner.asClass
      val effs = env.summaryOf(cls).effectsOf(sym)
      val outer = Outer(this, cls)(this.source)
      Effects.asSeenFrom(effs, this, cls, outer.toPots)
    }

    /** Potentials of a field, a method call or a lazy val access
     *
     *  The method performs prefix and outer substitution
     */
    def potentialsOf(sym: Symbol)(implicit env: Env): Potentials = trace("potentials of " + sym.show, init, r => Potentials.show(r.asInstanceOf)) {
      val cls = sym.owner.asClass
      val pots = env.summaryOf(cls).potentialsOf(sym)
      val outer = Outer(this, cls)(this.source)
      Potentials.asSeenFrom(pots, this, cls, outer.toPots)
    }

    private val outerCache: mutable.Map[ClassSymbol, Potentials] = mutable.Map.empty
    def outerFor(cls: ClassSymbol)(implicit env: Env): Potentials =
      if (outerCache.contains(cls)) outerCache(cls)
      else if (cls `eq` classSymbol) outer.toPots
      else {
        val bottomClsSummary = env.summaryOf(classSymbol)
        val objPart = ObjectPart(this, classSymbol, outer.toPots, bottomClsSummary.parentOuter)
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
    def show(using Context): String = "Outer[" + pot.show + ", " + classSymbol.show + "]"
  }

  /** The object pointed by `this.f` */
  case class FieldReturn(potential: Potential, field: Symbol)(val source: Tree) extends Potential {
    assert(field != NoSymbol)

    def size: Int = potential.size + 1
    def show(using Context): String = potential.show + "." + field.name.show
  }

  /** The object returned by `this.m()` */
  case class MethodReturn(potential: Potential, method: Symbol)(val source: Tree) extends Potential {
    assert(method != NoSymbol)

    def size: Int = potential.size + 1
    def show(using Context): String = potential.show + "." + method.name.show
  }

  /** The object whose initialization status is unknown */
  case class Cold()(val source: Tree) extends Potential {
    def size: Int = 1
    def show(using Context): String = "Cold"
  }

  /** A function when called will produce the `effects` and return the `potentials` */
  case class Fun(potentials: Potentials, effects: Effects)(val source: Tree) extends Potential {
    def size: Int = 1
    def show(using Context): String =
      "Fun[pots = " + potentials.map(_.show).mkString(";") + ", effs = " + effects.map(_.show).mkString(";") + "]"
  }

  // ------------------ operations on potentials ------------------

  extension (pot: Potential) def toPots: Potentials = Potentials.empty + pot

  extension (ps: Potentials) def select (symbol: Symbol, source: Tree)(using Context): Summary =
    ps.foldLeft(Summary.empty) { case ((pots, effs), pot) =>
      // max potential length
      // TODO: it can be specified on a project basis via compiler options
      if (pot.size > 2)
        (pots, effs + Promote(pot)(source))
      else if (symbol.isConstructor)
        (pots + pot, effs + MethodCall(pot, symbol)(source))
      else if (symbol.isOneOf(Flags.Method | Flags.Lazy))
          (
            pots + MethodReturn(pot, symbol)(source),
            effs + MethodCall(pot, symbol)(source)
          )
      else
        (pots + FieldReturn(pot, symbol)(source), effs + FieldAccess(pot, symbol)(source))
    }

  extension (ps: Potentials) def promote(source: Tree): Effects = ps.map(Promote(_)(source))

  def asSeenFrom(pot: Potential, thisValue: Potential, currentClass: ClassSymbol, outer: Potentials)(implicit env: Env): Potentials =
    trace(pot.show + " asSeenFrom " + thisValue.show + ", current = " + currentClass.show + ", outer = " + show(outer), init, pots => show(pots.asInstanceOf[Potentials])) { pot match {
      case MethodReturn(pot1, sym) =>
        val pots = asSeenFrom(pot1, thisValue, currentClass, outer)
        pots.map { MethodReturn(_, sym)(pot.source) }

      case FieldReturn(pot1, sym) =>
        val pots = asSeenFrom(pot1, thisValue, currentClass, outer)
        pots.map { FieldReturn(_, sym)(pot.source) }

      case Outer(pot1, cls) =>
        val pots = asSeenFrom(pot1, thisValue, currentClass, outer)
        pots map { Outer(_, cls)(pot.source) }

      case ThisRef(cls) =>
        if (cls `eq` currentClass)
          thisValue.toPots
        else if (currentClass.is(Flags.Package))
          Potentials.empty
        else {
          val outerCls = currentClass.owner.enclosingClass.asClass
          outer.flatMap { out =>
            asSeenFrom(pot, out, outerCls, Outer(out, outerCls)(out.source).toPots)
          }
        }

      case Fun(pots, effs) =>
        val pots1 = Potentials.asSeenFrom(pots, thisValue, currentClass, outer)
        val effs1 = Effects.asSeenFrom(effs, thisValue, currentClass, outer)
        Fun(pots1, effs1)(pot.source).toPots

      case Warm(cls, outer2) =>
        // widening to terminate
        val thisValue2 = thisValue match {
          case Warm(cls, outer) => Warm(cls, Cold()(outer.source))(thisValue.source)
          case _                => thisValue
        }

        val outer3 = asSeenFrom(outer2, thisValue2, currentClass, outer)
        outer3.map { Warm(cls, _)(pot.source) }

      case _: Cold =>
        pot.toPots

      case SuperRef(pot, supercls) =>
        val pots = asSeenFrom(pot, thisValue, currentClass, outer)
        pots.map { SuperRef(_, supercls)(pot.source) }
    } }

  def asSeenFrom(pots: Potentials, thisValue: Potential, currentClass: ClassSymbol, outer: Potentials)(implicit env: Env): Potentials =
    pots.flatMap(asSeenFrom(_, thisValue, currentClass, outer))
}