package dotty.tools.dotc
package transform.localopt

import core.Constants.Constant
import core.Contexts.Context
import core.StdNames._
import core.Symbols._
import core.Types._
import core.Flags._
import ast.Trees._
import transform.SymUtils._
import Simplify.desugarIdent
import dotty.tools.dotc.ast.tpd

/** Inline case class specific methods using desugarings assumptions.
 *
 *  Note: to run this optimisation after erasure one would need to specialize
 *  it for constructor with outer pointer and values classes. There is
 *  probably no need to run this more than once.
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
class InlineCaseIntrinsics(val simplifyPhase: Simplify) extends Optimisation {
  import ast.tpd._

  def visitor(implicit ctx: Context): Tree => Unit = NoVisitor
  def clear(): Unit = ()

  def transformer(implicit ctx: Context): Tree => Tree = {
    // For synthetic applies on case classes (both dotty/scalac)
    // - CC.apply(args) → new CC(args)
    case a: Apply
      if !a.tpe.isInstanceOf[MethodicType]           &&
         a.symbol.is(Synthetic)                      &&
         a.symbol.owner.is(Module)                   &&
         a.symbol.name == nme.apply                  &&
         a.symbol.owner.companionClass.is(CaseClass) &&
         !a.tpe.derivesFrom(defn.EnumClass)          &&
         (isPureExpr(a.fun) || a.fun.symbol.is(Synthetic)) =>

      def unrollArgs(t: Tree, l: List[List[Tree]]): List[List[Tree]] = t match {
        case Apply(t, args) => unrollArgs(t, args :: l)
        case _ => l
      }
      val argss = unrollArgs(a.fun, a.args :: Nil)
      def rollInArgs(l: List[List[Tree]], fun: Tree): Tree = l match {
        case head :: tail => rollInArgs(tail, fun.appliedToArgs(head))
        case _ => fun
      }
      val constructor = a.symbol.owner.companionClass.primaryConstructor.asTerm
      evalreceiver(a, rollInArgs(argss.tail, New(a.tpe.widenDealias.simplified, constructor, argss.head)))

    // For synthetic dotty unapplies on case classes:
    // - CC.unapply(arg): CC → arg
    // - CC.unapply(arg): Boolean → true, dotty only
    // - CC.unapply(arg): Option[CC] → new Some(new scala.TupleN(arg._1, ..., arg._N))
    case a: Apply
      if a.symbol.is(Synthetic)                       &&
         a.symbol.owner.is(Module)                    &&
         a.symbol.name == nme.unapply                 &&
         a.symbol.owner.companionClass.is(CaseClass)  &&
         !a.tpe.derivesFrom(defn.EnumClass)           &&
         (isPureExpr(a.fun) || a.fun.symbol.is(Synthetic)) =>

      val args = a.args.head
      val isDottyUnapply = !a.symbol.owner.is(Scala2x)
      val isScalaOptionUnapply =
        a.tpe.derivesFrom(defn.OptionClass) &&
        a.args.head.tpe.derivesFrom(a.symbol.owner.companionClass)

      if (isDottyUnapply) { // dotty only
        if (a.tpe.derivesFrom(defn.BooleanClass))
          // CC.unapply(arg): Boolean → true
          evalreceiver(a, Literal(Constant(true)))
        else
          // CC.unapply(arg): CC → arg
          evalreceiver(a, a.args.head)
      }
      else if (isScalaOptionUnapply) {
        // CC.unapply(arg): Option[CC] → new Some(new scala.TupleN(arg._1, ..., arg._N))
        // The output is defined as a Tree => Tree to go thought tpd.evalOnce.
        def some(e: Tree) = {
          val accessors = e.tpe.widenDealias.classSymbol.caseAccessors.filter(_.is(Method))
          val fields    = accessors.map(x => e.select(x).ensureApplied)
          val tplType   = a.tpe.baseArgTypes(defn.OptionClass).head
          val someTpe   = a.tpe.translateParameterized(defn.OptionClass, defn.SomeClass)

          if (fields.tail.nonEmpty)
            New(someTpe, New(tplType, fields) :: Nil)
          else // scalac does not have Tuple1
            New(someTpe, fields.head :: Nil)
        }
        val none = ref(defn.NoneModuleRef)
        def isNull(e: Tree) = e.select(defn.Object_eq).appliedTo(Literal(Constant(null)))
        def fi(e: Tree) = If(isNull(e), none, some(e))
        evalreceiver(a, evalOnce(a.args.head)(fi))
      }
      else a

    // Seq.unapplySeq(arg) → new Some(arg)
    // Where Seq is any companion of type <: SeqFactoryClass
    case a: Apply
      if a.symbol.name == nme.unapplySeq                  &&
         a.symbol.owner.derivesFrom(simplifyPhase.SeqFactoryClass) &&
         a.symbol.extendedOverriddenSymbols.isEmpty       &&
         (isPureExpr(a.fun) || a.fun.symbol.is(Synthetic)) =>

      def receiver(t: Tree): Type = t match {
        case t: Apply     => receiver(t.fun)
        case t: TypeApply => receiver(t.fun)
        case t: Ident     => desugarIdent(t) match {
          case Some(t) => receiver(t)
          case _ => NoType
        }
        case t: Select => t.qualifier.tpe.widenDealias
      }

      val recv = receiver(a)
      if (recv.typeSymbol.is(Module)) {
        val someTpe  = a.tpe.translateParameterized(defn.OptionClass, defn.SomeClass)
        evalreceiver(a, New(someTpe, a.args.head :: Nil))
      }
      else a
    case t => t
  }

  // Apply fun may be a side-effectful function. E.g. a block, see tests/run/t4859.scala
  // we need to maintain expressions that were in this block
  def evalreceiver(a: Apply, res: Tree)(implicit ctx: Context) = {
    def receiver(t: Tree): Tree = t match {
      case TypeApply(fun, targs) if fun.symbol eq t.symbol => receiver(fun)
      case Apply(fn, args) if fn.symbol == t.symbol => receiver(fn)
      case Select(qual, _) => qual
      case x => x
    }
    val recv = receiver(a)
    if (recv.isEmpty || isPureRef(recv))
      res
    else
      Block(recv :: Nil, res)
  }
}
