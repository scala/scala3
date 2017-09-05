package dotty.tools.dotc
package transform

import core._
import Symbols._, Types._, Contexts._, Names._, StdNames._, Constants._, SymUtils._
import scala.collection.{ mutable, immutable }
import Flags._
import TreeTransforms._
import DenotTransformers._
import ast.Trees._
import ast.untpd
import Decorators._
import NameOps._
import ValueClasses.isDerivedValueClass
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

/** Synthetic method implementations for case classes, case objects,
 *  and value classes.
 *
 *  Selectively added to case classes/objects, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 *    def canEqual(other: Any): Boolean
 *    def toString(): String
 *    def productElement(i: Int): Any
 *    def productArity: Int
 *    def productPrefix: String
 *
 *  Special handling:
 *    protected def readResolve(): AnyRef
 *
 *  Selectively added to value classes, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 */
class SyntheticMethods(thisTransformer: DenotTransformer) {
  import ast.tpd._

  private var myValueSymbols: List[Symbol] = Nil
  private var myCaseSymbols: List[Symbol] = Nil

  private def initSymbols(implicit ctx: Context) =
    if (myValueSymbols.isEmpty) {
      myValueSymbols = List(defn.Any_hashCode, defn.Any_equals)
      myCaseSymbols = myValueSymbols ++ List(defn.Any_toString, defn.Product_canEqual,
        defn.Product_productArity, defn.Product_productPrefix, defn.Product_productElement)
    }

  def valueSymbols(implicit ctx: Context) = { initSymbols; myValueSymbols }
  def caseSymbols(implicit ctx: Context) = { initSymbols; myCaseSymbols }

  /** The synthetic methods of the case or value class `clazz`. */
  def syntheticMethods(clazz: ClassSymbol)(implicit ctx: Context): List[Tree] = {
    val clazzType = clazz.typeRef
    lazy val accessors =
      if (isDerivedValueClass(clazz))
        clazz.termParamAccessors
      else
        clazz.caseAccessors

    val symbolsToSynthesize: List[Symbol] =
      if (clazz.is(Case)) caseSymbols
      else if (isDerivedValueClass(clazz)) valueSymbols
      else Nil

    def syntheticDefIfMissing(sym: Symbol): List[Tree] = {
      val existing = sym.matchingMember(clazz.thisType)
      if (existing == sym || existing.is(Deferred)) syntheticDef(sym) :: Nil
      else Nil
    }

    def syntheticDef(sym: Symbol): Tree = {
      val synthetic = sym.copy(
        owner = clazz,
        flags = sym.flags &~ Deferred | Synthetic | Override,
        coord = clazz.coord).enteredAfter(thisTransformer).asTerm

      def forwardToRuntime(vrefss: List[List[Tree]]): Tree =
        ref(defn.runtimeMethodRef("_" + sym.name.toString)).appliedToArgs(This(clazz) :: vrefss.head)

      def ownName(vrefss: List[List[Tree]]): Tree =
        Literal(Constant(clazz.name.stripModuleClassSuffix.toString))

      def syntheticRHS(implicit ctx: Context): List[List[Tree]] => Tree = synthetic.name match {
        case nme.hashCode_ if isDerivedValueClass(clazz) => vrefss => valueHashCodeBody
        case nme.hashCode_ => vrefss => caseHashCodeBody
        case nme.toString_ => if (clazz.is(ModuleClass)) ownName else forwardToRuntime
        case nme.equals_ => vrefss => equalsBody(vrefss.head.head)
        case nme.canEqual_ => vrefss => canEqualBody(vrefss.head.head)
        case nme.productArity => vrefss => Literal(Constant(accessors.length))
        case nme.productPrefix => ownName
        case nme.productElement => vrefss => productElementBody(accessors.length, vrefss.head.head)
      }
      ctx.log(s"adding $synthetic to $clazz at ${ctx.phase}")
      DefDef(synthetic, syntheticRHS(ctx.withOwner(synthetic)))
    }

    /** The class
     *
     *  ```
     *  case class C(x: T, y: T)
     *  ```
     *
     *  gets the `productElement` method:
     *
     *  ```
     *  def productElement(index: Int): Any = index match {
     *    case 0 => this._1
     *    case 1 => this._2
     *    case _ => throw new IndexOutOfBoundsException(index.toString)
     *  }
     *  ```
     */
    def productElementBody(arity: Int, index: Tree)(implicit ctx: Context): Tree = {
      val ioob = defn.IndexOutOfBoundsException.typeRef
      // Second constructor of ioob that takes a String argument
      def filterStringConstructor(s: Symbol): Boolean = s.info match {
        case m: MethodType if s.isConstructor => m.paramInfos == List(defn.StringType)
        case _ => false
      }
      val constructor = ioob.typeSymbol.info.decls.find(filterStringConstructor _).asTerm
      val stringIndex = Apply(Select(index, nme.toString_), Nil)
      val error = Throw(New(ioob, constructor, List(stringIndex)))

      // case _ => throw new IndexOutOfBoundsException(i.toString)
      val defaultCase = CaseDef(Underscore(defn.IntType), EmptyTree, error)

      // case N => _${N + 1}
      val cases = 0.until(arity).map { i =>
        CaseDef(Literal(Constant(i)), EmptyTree, Select(This(clazz), nme.selectorName(i)))
      }

      Match(index, (cases :+ defaultCase).toList)
    }

    /** The class
     *
     *  ```
     *  case class C(x: T, y: U)
     *  ```
     *
     *  gets the `equals` method:
     *
     *  ```
     *  def equals(that: Any): Boolean =
     *    (this eq that) || {
     *      that match {
     *        case x$0 @ (_: C) => this.x == this$0.x && this.y == x$0.y
     *        case _ => false
     *     }
     *  ```
     *
     *  If `C` is a value class the initial `eq` test is omitted.
     */
    def equalsBody(that: Tree)(implicit ctx: Context): Tree = {
      val thatAsClazz = ctx.newSymbol(ctx.owner, nme.x_0, Synthetic, clazzType, coord = ctx.owner.pos) // x$0
      def wildcardAscription(tp: Type) = Typed(Underscore(tp), TypeTree(tp))
      val pattern = Bind(thatAsClazz, wildcardAscription(clazzType)) // x$0 @ (_: C)
      val comparisons = accessors collect { case accessor if !accessor.info.isPhantom =>
        This(clazz).select(accessor).select(defn.Any_==).appliedTo(ref(thatAsClazz).select(accessor)) }
      val rhs = // this.x == this$0.x && this.y == x$0.y
        if (comparisons.isEmpty) Literal(Constant(true)) else comparisons.reduceLeft(_ and _)
      val matchingCase = CaseDef(pattern, EmptyTree, rhs) // case x$0 @ (_: C) => this.x == this$0.x && this.y == x$0.y
      val defaultCase = CaseDef(wildcardAscription(defn.AnyType), EmptyTree, Literal(Constant(false))) // case _ => false
      val matchExpr = Match(that, List(matchingCase, defaultCase))
      if (isDerivedValueClass(clazz)) matchExpr
      else {
        val eqCompare = This(clazz).select(defn.Object_eq).appliedTo(that.asInstance(defn.ObjectType))
        eqCompare or matchExpr
      }
    }

    /** The class
     *
     *  ```
     *  class C(x: T) extends AnyVal
     *  ```
     *
     *  gets the `hashCode` method:
     *
     *  ```
     *  def hashCode: Int = x.hashCode()
     *  ```
     */
    def valueHashCodeBody(implicit ctx: Context): Tree = {
      assert(accessors.nonEmpty)
      assert(accessors.tail.forall(_.info.isPhantom))
      ref(accessors.head).select(nme.hashCode_).ensureApplied
    }

    /** The class
     *
     *  ```
     *  package p
     *  case class C(x: T, y: T)
     *  ```
     *
     *  gets the `hashCode` method:
     *
     *  ```
     *  def hashCode: Int = {
     *    <synthetic> var acc: Int = "p.C".hashCode // constant folded
     *    acc = Statics.mix(acc, x);
     *    acc = Statics.mix(acc, Statics.this.anyHash(y));
     *    Statics.finalizeHash(acc, 2)
     *  }
     *  ```
     */
    def caseHashCodeBody(implicit ctx: Context): Tree = {
      val acc = ctx.newSymbol(ctx.owner, "acc".toTermName, Mutable | Synthetic, defn.IntType, coord = ctx.owner.pos)
      val accDef = ValDef(acc, Literal(Constant(clazz.fullName.toString.hashCode)))
      val mixes = for (accessor <- accessors.toList) yield
        Assign(ref(acc), ref(defn.staticsMethod("mix")).appliedTo(ref(acc), hashImpl(accessor)))
      val finish = ref(defn.staticsMethod("finalizeHash")).appliedTo(ref(acc), Literal(Constant(accessors.size)))
      Block(accDef :: mixes, finish)
    }

    /** The `hashCode` implementation for given symbol `sym`. */
    def hashImpl(sym: Symbol)(implicit ctx: Context): Tree =
      defn.scalaClassName(sym.info.finalResultType) match {
        case tpnme.Unit | tpnme.Null               => Literal(Constant(0))
        case tpnme.Boolean                         => If(ref(sym), Literal(Constant(1231)), Literal(Constant(1237)))
        case tpnme.Int                             => ref(sym)
        case tpnme.Short | tpnme.Byte | tpnme.Char => ref(sym).select(nme.toInt)
        case tpnme.Long                            => ref(defn.staticsMethod("longHash")).appliedTo(ref(sym))
        case tpnme.Double                          => ref(defn.staticsMethod("doubleHash")).appliedTo(ref(sym))
        case tpnme.Float                           => ref(defn.staticsMethod("floatHash")).appliedTo(ref(sym))
        case _                                     => ref(defn.staticsMethod("anyHash")).appliedTo(ref(sym))
      }

    /** The class
     *
     *  ```
     *  case class C(...)
     *  ```
     *
     *  gets the `canEqual` method
     *
     *  ```
     *  def canEqual(that: Any) = that.isInstanceOf[C]
     *  ```
     */
    def canEqualBody(that: Tree): Tree = that.isInstance(clazzType)

    symbolsToSynthesize flatMap syntheticDefIfMissing
  }

  def addSyntheticMethods(impl: Template)(implicit ctx: Context) =
    if (ctx.owner.is(Case) || isDerivedValueClass(ctx.owner))
      cpy.Template(impl)(body = impl.body ++ syntheticMethods(ctx.owner.asClass))
    else
      impl
}
