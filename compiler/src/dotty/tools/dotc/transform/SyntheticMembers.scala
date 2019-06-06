package dotty.tools.dotc
package transform

import core._
import Symbols._, Types._, Contexts._, Names._, StdNames._, Constants._, SymUtils._
import Flags._
import DenotTransformers._
import Decorators._
import NameOps._
import Annotations.Annotation
import typer.ProtoTypes.constrained
import ast.untpd
import ValueClasses.isDerivedValueClass
import SymUtils._
import util.Property
import config.Printers.derive

object SyntheticMembers {

  /** Attachment marking an anonymous class as a singleton case that will extend from Mirror.Singleton */
  val ExtendsSingletonMirror: Property.StickyKey[Unit] = new Property.StickyKey

  /** Attachment recording that an anonymous class should extend Mirror.Product */
  val ExtendsProductMirror: Property.StickyKey[Unit] = new Property.StickyKey

  /** Attachment recording that an anonymous class should extend Mirror.Sum */
  val ExtendsSumMirror: Property.StickyKey[Unit] = new Property.StickyKey
}

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
 *  Add to serializable static objects, unless an implementation
 *  already exists:
 *    private def writeReplace(): AnyRef
 *
 *  Selectively added to value classes, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 */
class SyntheticMembers(thisPhase: DenotTransformer) {
  import SyntheticMembers._
  import ast.tpd._

  private[this] var myValueSymbols: List[Symbol] = Nil
  private[this] var myCaseSymbols: List[Symbol] = Nil
  private[this] var myCaseModuleSymbols: List[Symbol] = Nil
  private[this] var myEnumCaseSymbols: List[Symbol] = Nil

  private def initSymbols(implicit ctx: Context) =
    if (myValueSymbols.isEmpty) {
      myValueSymbols = List(defn.Any_hashCode, defn.Any_equals)
      myCaseSymbols = myValueSymbols ++ List(defn.Any_toString, defn.Product_canEqual,
        defn.Product_productArity, defn.Product_productPrefix, defn.Product_productElement)
      myCaseModuleSymbols = myCaseSymbols.filter(_ ne defn.Any_equals)
      myEnumCaseSymbols = List(defn.Enum_ordinal)
    }

  def valueSymbols(implicit ctx: Context): List[Symbol] = { initSymbols; myValueSymbols }
  def caseSymbols(implicit ctx: Context): List[Symbol] = { initSymbols; myCaseSymbols }
  def caseModuleSymbols(implicit ctx: Context): List[Symbol] = { initSymbols; myCaseModuleSymbols }
  def enumCaseSymbols(implicit ctx: Context): List[Symbol] = { initSymbols; myEnumCaseSymbols }

  private def existingDef(sym: Symbol, clazz: ClassSymbol)(implicit ctx: Context): Symbol = {
    val existing = sym.matchingMember(clazz.thisType)
    if (existing != sym && !existing.is(Deferred)) existing
    else NoSymbol
  }

  private def synthesizeDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Context => Tree)(implicit ctx: Context): Tree =
    DefDef(sym, rhsFn(_)(ctx.withOwner(sym))).withSpan(ctx.owner.span.focus)

  /** If this is a case or value class, return the appropriate additional methods,
   *  otherwise return nothing.
   */
  def caseAndValueMethods(clazz: ClassSymbol)(implicit ctx: Context): List[Tree] = {
    val clazzType = clazz.appliedRef
    lazy val accessors =
      if (isDerivedValueClass(clazz)) clazz.paramAccessors.take(1) // Tail parameters can only be `erased`
      else clazz.caseAccessors
    val isEnumCase = clazz.derivesFrom(defn.EnumClass) && clazz != defn.EnumClass

    val symbolsToSynthesize: List[Symbol] =
      if (clazz.is(Case)) {
        if (clazz.is(Module)) caseModuleSymbols
        else if (isEnumCase) caseSymbols ++ enumCaseSymbols
        else caseSymbols
      }
      else if (isEnumCase) enumCaseSymbols
      else if (isDerivedValueClass(clazz)) valueSymbols
      else Nil

    def syntheticDefIfMissing(sym: Symbol): List[Tree] =
      if (existingDef(sym, clazz).exists) Nil else syntheticDef(sym) :: Nil

    def syntheticDef(sym: Symbol): Tree = {
      val synthetic = sym.copy(
        owner = clazz,
        flags = sym.flags &~ Deferred | Synthetic | Override,
        info = clazz.thisType.memberInfo(sym),
        coord = clazz.coord).enteredAfter(thisPhase).asTerm

      def forwardToRuntime(vrefs: List[Tree]): Tree =
        ref(defn.runtimeMethodRef("_" + sym.name.toString)).appliedToArgs(This(clazz) :: vrefs)

      def ownName: Tree =
        Literal(Constant(clazz.name.stripModuleClassSuffix.toString))

      def syntheticRHS(vrefss: List[List[Tree]])(implicit ctx: Context): Tree = synthetic.name match {
        case nme.hashCode_ if isDerivedValueClass(clazz) => valueHashCodeBody
        case nme.hashCode_ => caseHashCodeBody
        case nme.toString_ => if (clazz.is(ModuleClass)) ownName else forwardToRuntime(vrefss.head)
        case nme.equals_ => equalsBody(vrefss.head.head)
        case nme.canEqual_ => canEqualBody(vrefss.head.head)
        case nme.productArity => Literal(Constant(accessors.length))
        case nme.productPrefix => ownName
        case nme.productElement => productElementBody(accessors.length, vrefss.head.head)
        case nme.ordinal => Select(This(clazz), nme.ordinalDollar)
      }
      ctx.log(s"adding $synthetic to $clazz at ${ctx.phase}")
      synthesizeDef(synthetic, treess => ctx => syntheticRHS(treess)(ctx))
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
     *        case x$0 @ (_: C @unchecked) => this.x == this$0.x && this.y == x$0.y
     *        case _ => false
     *     }
     *  ```
     *
     *  If `C` is a value class the initial `eq` test is omitted.
     *
     *  `@unchecked` is needed for parametric case classes.
     *
     */
    def equalsBody(that: Tree)(implicit ctx: Context): Tree = {
      val thatAsClazz = ctx.newSymbol(ctx.owner, nme.x_0, Synthetic, clazzType, coord = ctx.owner.span) // x$0
      def wildcardAscription(tp: Type) = Typed(Underscore(tp), TypeTree(tp))
      val pattern = Bind(thatAsClazz, wildcardAscription(AnnotatedType(clazzType, Annotation(defn.UncheckedAnnot)))) // x$0 @ (_: C @unchecked)
      // compare primitive fields first, slow equality checks of non-primitive fields can be skipped when primitives differ
      val sortedAccessors = accessors.sortBy(accessor => if (accessor.info.typeSymbol.isPrimitiveValueClass) 0 else 1)
      val comparisons = sortedAccessors.map { accessor =>
        This(clazz).select(accessor).equal(ref(thatAsClazz).select(accessor)) }
      val rhs = // this.x == this$0.x && this.y == x$0.y
        if (comparisons.isEmpty) Literal(Constant(true)) else comparisons.reduceLeft(_ and _)
      val matchingCase = CaseDef(pattern, EmptyTree, rhs) // case x$0 @ (_: C) => this.x == this$0.x && this.y == x$0.y
      val defaultCase = CaseDef(Underscore(defn.AnyType), EmptyTree, Literal(Constant(false))) // case _ => false
      val matchExpr = Match(that, List(matchingCase, defaultCase))
      if (isDerivedValueClass(clazz)) matchExpr
      else {
        val eqCompare = This(clazz).select(defn.Object_eq).appliedTo(that.cast(defn.ObjectType))
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
      val seed = clazz.fullName.toString.hashCode
      if (accessors.nonEmpty) {
        val acc = ctx.newSymbol(ctx.owner, "acc".toTermName, Mutable | Synthetic, defn.IntType, coord = ctx.owner.span)
        val accDef = ValDef(acc, Literal(Constant(seed)))
        val mixes = for (accessor <- accessors) yield
          Assign(ref(acc), ref(defn.staticsMethod("mix")).appliedTo(ref(acc), hashImpl(accessor)))
        val finish = ref(defn.staticsMethod("finalizeHash")).appliedTo(ref(acc), Literal(Constant(accessors.size)))
        Block(accDef :: mixes, finish)
      } else {
        // Pre-compute the hash code
        val hash = scala.runtime.Statics.finalizeHash(seed, 0)
        Literal(Constant(hash))
      }
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
     *  def canEqual(that: Any) = that.isInstanceOf[C @unchecked]
     *  ```
     *
     *  `@unchecked` is needed for parametric case classes.
     */
    def canEqualBody(that: Tree): Tree = that.isInstance(AnnotatedType(clazzType, Annotation(defn.UncheckedAnnot)))

    symbolsToSynthesize.flatMap(syntheticDefIfMissing)
  }

  /** If this is a serializable static object `Foo`, add the method:
   *
   *      private def writeReplace(): AnyRef =
   *        new scala.runtime.ModuleSerializationProxy(classOf[Foo.type])
   *
   *  unless an implementation already exists, otherwise do nothing.
   */
  def serializableObjectMethod(clazz: ClassSymbol)(implicit ctx: Context): List[Tree] = {
    def hasWriteReplace: Boolean =
      clazz.membersNamed(nme.writeReplace)
        .filterWithPredicate(s => s.signature == Signature(defn.AnyRefType, isJava = false))
        .exists
    if (clazz.is(Module) && clazz.isStatic && clazz.isSerializable && !hasWriteReplace) {
      val writeReplace = ctx.newSymbol(clazz, nme.writeReplace, Method | Private | Synthetic,
        MethodType(Nil, defn.AnyRefType), coord = clazz.coord).entered.asTerm
      List(
        DefDef(writeReplace,
          _ => New(defn.ModuleSerializationProxyType,
                   defn.ModuleSerializationProxyConstructor,
                   List(Literal(Constant(clazz.sourceModule.termRef)))))
          .withSpan(ctx.owner.span.focus))
    }
    else
      Nil
  }

   /** The class
     *
     *  ```
     *  case class C[T <: U](x: T, y: String*)
     *  ```
     *
     *  gets the `fromProduct` method:
     *
     *  ```
     *  def fromProduct(x$0: Product): MirroredMonoType =
     *    new C[U](
     *      x$0.productElement(0).asInstanceOf[U],
     *      x$0.productElement(1).asInstanceOf[Seq[String]]: _*)
     *  ```
     *  where
     *  ```
     *  type MirroredMonoType = C[_]
     *  ```
     */
    def fromProductBody(caseClass: Symbol, param: Tree)(implicit ctx: Context): Tree = {
      val (classRef, methTpe) =
        caseClass.primaryConstructor.info match {
          case tl: PolyType =>
            val (tl1, tpts) = constrained(tl, untpd.EmptyTree, alwaysAddTypeVars = true)
            val targs =
              for (tpt <- tpts) yield
                tpt.tpe match {
                  case tvar: TypeVar => tvar.instantiate(fromBelow = false)
                }
            (caseClass.typeRef.appliedTo(targs), tl.instantiate(targs))
          case methTpe =>
            (caseClass.typeRef, methTpe)
        }
      methTpe match {
        case methTpe: MethodType =>
          val elems =
            for ((formal, idx) <- methTpe.paramInfos.zipWithIndex) yield {
              val elem =
                param.select(defn.Product_productElement).appliedTo(Literal(Constant(idx)))
                  .ensureConforms(formal.underlyingIfRepeated(isJava = false))
               if (formal.isRepeatedParam) ctx.typer.seqToRepeated(elem) else elem
            }
          New(classRef, elems)
      }
    }

  /** For an enum T:
   *
   *     def ordinal(x: MirroredMonoType) = x.ordinal
   *
   *  For  sealed trait with children of normalized types C_1, ..., C_n:
   *
   *     def ordinal(x: MirroredMonoType) = x match {
   *        case _: C_1 => 0
   *        ...
   *        case _: C_n => n - 1
   *     }
   *
   *  Here, the normalized type of a class C is C[_, ...., _] with
   *  a wildcard for each type parameter. The normalized type of an object
   *  O is O.type.
   */
  def ordinalBody(cls: Symbol, param: Tree)(implicit ctx: Context): Tree =
    if (cls.is(Enum)) param.select(nme.ordinal).ensureApplied
    else {
      val cases =
        for ((child, idx) <- cls.children.zipWithIndex) yield {
          val patType = if (child.isTerm) child.termRef else child.rawTypeRef
          val pat = Typed(untpd.Ident(nme.WILDCARD).withType(patType), TypeTree(patType))
          CaseDef(pat, EmptyTree, Literal(Constant(idx)))
        }
      Match(param, cases)
    }

  /** - If `impl` is the companion of a generic sum, add `deriving.Mirror.Sum` parent
   *    and `MirroredMonoType` and `ordinal` members.
   *  - If `impl` is the companion of a generic product, add `deriving.Mirror.Product` parent
   *    and `MirroredMonoType` and `fromProduct` members.
   *  - If `impl` is marked with one of the attachments ExtendsSingletonMirror, ExtendsProductMirror,
   *    or ExtendsSumMirror, remove the attachment and generate the corresponding mirror support,
   *    On this case the represented class or object is referred to in a pre-existing `MirroredMonoType`
   *    member of the template.
   */
  def addMirrorSupport(impl: Template)(implicit ctx: Context): Template = {
    val clazz = ctx.owner.asClass

    var newBody = impl.body
    var newParents = impl.parents
    def addParent(parent: Type): Unit = {
      newParents = newParents :+ TypeTree(parent)
      val oldClassInfo = clazz.classInfo
      val newClassInfo = oldClassInfo.derivedClassInfo(
        classParents = oldClassInfo.classParents :+ parent)
      clazz.copySymDenotation(info = newClassInfo).installAfter(thisPhase)
    }
    def addMethod(name: TermName, info: Type, cls: Symbol, body: (Symbol, Tree, Context) => Tree): Unit = {
      val meth = ctx.newSymbol(clazz, name, Synthetic | Method, info, coord = clazz.coord)
      if (!existingDef(meth, clazz).exists) {
        meth.entered
        newBody = newBody :+
          synthesizeDef(meth, vrefss => ctx => body(cls, vrefss.head.head, ctx))
      }
    }
    val linked = clazz.linkedClass
    lazy val monoType = {
      val existing = clazz.info.member(tpnme.MirroredMonoType).symbol
      if (existing.exists && !existing.is(Deferred)) existing
      else {
        val monoType =
          ctx.newSymbol(clazz, tpnme.MirroredMonoType, Synthetic, TypeAlias(linked.rawTypeRef), coord = clazz.coord)
        newBody = newBody :+ TypeDef(monoType).withSpan(ctx.owner.span.focus)
        monoType.entered
      }
    }
    def makeSingletonMirror() =
      addParent(defn.Mirror_SingletonType)
    def makeProductMirror(cls: Symbol) = {
      addParent(defn.Mirror_ProductType)
      addMethod(nme.fromProduct, MethodType(defn.ProductType :: Nil, monoType.typeRef), cls,
        fromProductBody(_, _)(_).ensureConforms(monoType.typeRef))  // t4758.scala or i3381.scala are examples where a cast is needed
    }
    def makeSumMirror(cls: Symbol) = {
      addParent(defn.Mirror_SumType)
      addMethod(nme.ordinal, MethodType(monoType.typeRef :: Nil, defn.IntType), cls,
        ordinalBody(_, _)(_))
    }

    if (clazz.is(Module)) {
      if (clazz.is(Case)) makeSingletonMirror()
      else if (linked.isGenericProduct) makeProductMirror(linked)
      else if (linked.isGenericSum) makeSumMirror(linked)
      else if (linked.is(Sealed))
        derive.println(i"$linked is not a sum because ${linked.whyNotGenericSum}")
    }
    else if (impl.removeAttachment(ExtendsSingletonMirror).isDefined)
      makeSingletonMirror()
    else if (impl.removeAttachment(ExtendsProductMirror).isDefined)
      makeProductMirror(monoType.typeRef.dealias.classSymbol)
    else if (impl.removeAttachment(ExtendsSumMirror).isDefined)
      makeSumMirror(monoType.typeRef.dealias.classSymbol)

    cpy.Template(impl)(parents = newParents, body = newBody)
  }

  def addSyntheticMembers(impl: Template)(implicit ctx: Context): Template = {
    val clazz = ctx.owner.asClass
    addMirrorSupport(
      cpy.Template(impl)(body = serializableObjectMethod(clazz) ::: caseAndValueMethods(clazz) ::: impl.body))
  }
}
