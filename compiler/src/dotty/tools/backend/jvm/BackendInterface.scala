package dotty.tools
package backend
package jvm

import scala.collection.generic.Clearable
import scala.reflect.ClassTag
import dotty.tools.io.AbstractFile
import scala.language.implicitConversions
import scala.tools.asm

/* Interface to abstract over frontend inside backend.
 * Intended to be implemented by both scalac and dotc
 */
abstract class BackendInterface extends BackendInterfaceDefinitions {
  type Flags      = Long

  type Constant   >: Null <: AnyRef
  type Symbol     >: Null <: AnyRef
  type Type       >: Null <: AnyRef
  type Annotation >: Null <: AnyRef
  type Tree       >: Null <: AnyRef
  type Modifiers  >: Null <: AnyRef
  type TypeDef    >: Null <: Tree
  type Apply      >: Null <: Tree
  type Select     >: Null <: Tree
  type TypeApply  >: Null <: Tree
  type ClassDef   >: Null <: Tree
  type Try        >: Null <: Tree
  type Assign     >: Null <: Tree
  type Ident      >: Null <: Tree
  type If         >: Null <: Tree
  type LabelDef   >: Null <: Tree
  type ValDef     >: Null <: Tree
  type Throw      >: Null <: Tree
  type Labeled    >: Null <: Tree
  type Return     >: Null <: Tree
  type WhileDo    >: Null <: Tree
  type Literal    >: Null <: Tree
  type Block      >: Null <: Tree
  type Typed      >: Null <: Tree
  type ArrayValue >: Null <: Tree
  type Match      >: Null <: Tree
  type This       >: Null <: Tree
  type CaseDef    >: Null <: Tree
  type Alternative >: Null <: Tree
  type DefDef     >: Null <: Tree
  type ModuleDef  >: Null <: Tree
  type Template   >: Null <: Tree
  type Name       >: Null <: AnyRef
  type Position
  type CompilationUnit <: AnyRef
  type Bind         >: Null <: Tree
  type New          >: Null <: Tree
  type ApplyDynamic >: Null <: Tree
  type Super       >: Null <: Tree
  type Closure     >: Null <: Tree


  implicit val TypeDefTag: ClassTag[TypeDef]
  implicit val ApplyTag: ClassTag[Apply]
  implicit val SelectTag: ClassTag[Select]

  implicit val TypeApplyTag: ClassTag[TypeApply]
  implicit val ClassDefTag: ClassTag[ClassDef]
  implicit val TryTag: ClassTag[Try]
  implicit val AssignTag: ClassTag[Assign]
  implicit val IdentTag: ClassTag[Ident]
  implicit val IfTag: ClassTag[If]
  implicit val LabelDefTag: ClassTag[LabelDef]
  implicit val ValDefTag: ClassTag[ValDef]
  implicit val ThrowTag: ClassTag[Throw]
  implicit val LabeledTag: ClassTag[Labeled]
  implicit val ReturnTag: ClassTag[Return]
  implicit val WhileDoTag: ClassTag[WhileDo]
  implicit val LiteralTag: ClassTag[Literal]
  implicit val BlockTag: ClassTag[Block]
  implicit val TypedTag: ClassTag[Typed]
  implicit val ArrayValueTag: ClassTag[ArrayValue]
  implicit val MatchTag: ClassTag[Match]
  implicit val CaseDefTag: ClassTag[CaseDef]
  implicit val ThisTag: ClassTag[This]
  implicit val AlternativeTag: ClassTag[Alternative]
  implicit val DefDefTag: ClassTag[DefDef]
  implicit val ModuleDefTag: ClassTag[ModuleDef]
  implicit val NameTag: ClassTag[Name]
  implicit val TemplateTag: ClassTag[Template]
  implicit val BindTag: ClassTag[Bind]
  implicit val NewTag: ClassTag[New]
  implicit val ApplyDynamicTag: ClassTag[ApplyDynamic]
  implicit val SuperTag: ClassTag[Super]
  implicit val ConstantClassTag: ClassTag[Constant]
  implicit val ClosureTag: ClassTag[Closure]

  type ConstantTag = Int

  val UnitTag: ConstantTag
  val IntTag: ConstantTag
  val FloatTag: ConstantTag
  val NullTag: ConstantTag
  val BooleanTag: ConstantTag
  val ByteTag: ConstantTag
  val ShortTag: ConstantTag
  val CharTag: ConstantTag
  val DoubleTag: ConstantTag
  val LongTag: ConstantTag
  val StringTag: ConstantTag
  val ClazzTag: ConstantTag
  val EnumTag: ConstantTag

  val primitives: Primitives


  val nme_This: Name
  val nme_EMPTY_PACKAGE_NAME: Name
  val nme_CONSTRUCTOR: Name
  val nme_WILDCARD: Name
  val nme_THIS: Name
  val nme_PACKAGE: Name
  val nme_EQEQ_LOCAL_VAR: Name
  val nme_apply: Name

  /* methods used to box&unbox primitives ?and value classes? */
  def boxMethods: Map[Symbol, Symbol] // (class, method)
  def unboxMethods: Map[Symbol, Symbol]

  /* dotty specific, see dotty.runtime.Arrays */
  def isSyntheticArrayConstructor(s: Symbol) = false

  /*
   * Collects all LabelDef nodes enclosed (directly or not) by each node.
   *
   * In other words, this prepares a map giving
   * all labelDefs (the entry-value) having a Tree node (the entry-key) as ancestor.
   * The entry-value for a LabelDef entry-key always contains the entry-key.
   */
  def getLabelDefOwners(t: Tree): Map[Tree, List[LabelDef]]

  /*
   * Implementation of those methods is very specific to how annotations are represented
   *  representations for Dotc and Scalac are to different to abstract over them
   */
  def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit
  def emitAnnotations(mw: asm.MethodVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit
  def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit
  def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[Annotation]], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit

  /* means of getting class symbols from compiler */
  def requiredClass[T: ClassTag]: Symbol
  def requiredModule[T: ClassTag]: Symbol
  def getRequiredClass(fullname: String): Symbol
  def getClassIfDefined(fullname: String): Symbol

  def isQualifierSafeToElide(qual: Tree): Boolean
  def desugarIdent(i: Ident): Option[Select]

  /* various configuration options used by backend */
  def emitAsmp: Option[String]
  def dumpClasses: Option[String]
  def mainClass: Option[String]
  def noForwarders: Boolean
  def debuglevel: Int
  def settings_debug: Boolean
  def targetPlatform: String
  def sourceFileFor(cu: CompilationUnit): String
  def setMainClass(name: String): Unit
  def informProgress(msg: String): Unit
  def hasLabelDefs: Boolean // whether this compiler uses LabelDefs (i.e., scalac)

  /* backend actually uses free names to generate stuff. This should NOT mangled */
  def newTermName(prefix: String): Name

  def getGenericSignature(sym: Symbol, owner:Symbol): String
  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String

  def isBox(sym: Symbol): Boolean
  def isUnbox(sym: Symbol): Boolean
  def isMaybeBoxed(sym: Symbol): Boolean

  /** Whether an annotation should be emitted as a Java annotation
    * .initialize: if 'annot' is read from pickle, atp might be un-initialized
    */
  def shouldEmitAnnotation(annot: Annotation): Boolean

  def isRuntimeVisible(annot: Annotation): Boolean

  def getSingleOutput: Option[AbstractFile]

  implicit def symHelper(sym: Symbol): SymbolHelper
  implicit def typeHelper(tp: Type): TypeHelper
  implicit def nameHelper(n: Name): NameHelper
  implicit def annotHelper(a: Annotation): AnnotationHelper
  implicit def treeHelper(a: Tree): TreeHelper

  implicit def constantHelper(a: Constant): ConstantHelper
  implicit def positionHelper(a: Position): PositionHelper


  val Assign: AssignDeconstructor
  val Select: SelectDeconstructor
  val Apply: ApplyDeconstructor
  val If: IfDeconstructor
  val ValDef: ValDefDeconstructor
  val Throw: ThrowDeconstructor
  val New: NewDeconstructor
  val ApplyDynamic: ApplyDynamicDeconstructor
  val This: ThisDeconstructor
  val Ident: IdentDeconstructor
  val Try: TryDeconstructor
  val Labeled: LabeledDeconstructor
  val Return: ReturnDeconstructor
  val WhileDo: WhileDoDeconstructor
  val LabelDef: LabelDeconstructor
  val Literal: LiteralDeconstructor
  val Typed: TypedDeconstrutor
  val Super: SuperDeconstructor
  val ArrayValue: ArrayValueDeconstructor
  val Match: MatchDeconstructor
  val Block: BlockDeconstructor
  val TypeApply: TypeApplyDeconstructor
  val CaseDef: CaseDeconstructor
  val Alternative: AlternativeDeconstructor
  val Constant: ConstantDeconstructor
  val ThrownException: ThrownException
  val DefDef: DefDefDeconstructor
  val ModuleDef: ModuleDefDeconstructor
  val Template: TemplateDeconstructor
  val Bind: BindDeconstructor
  val ClassDef: ClassDefDeconstructor
  val Closure: ClosureDeconstructor

  abstract class DeconstructorCommon[T >: Null <: AnyRef] {
    var field: T = null
    def get: this.type = this
    def isEmpty: Boolean = field eq null
    def isDefined = !isEmpty
    def unapply(s: T): this.type ={
      field = s
      this
    }
  }

  abstract class Deconstructor1Common[T >: Null <: AnyRef, R]{
    var field: T = _
    def get: R
    def isEmpty: Boolean = field eq null
    def isDefined = !isEmpty
    def unapply(s: T): this.type ={
      field = s
      this
    }
  }

  abstract class ClassDefDeconstructor extends DeconstructorCommon[ClassDef] {
    def _1: Modifiers
    def _2: Name
    def _3: List[TypeDef]
    def _4: Template
  }

  abstract class BindDeconstructor extends DeconstructorCommon[Bind]{
    def _1: Name
    def _2: Tree
  }

  abstract class TemplateDeconstructor extends DeconstructorCommon[Template]{
    def _1: List[Tree]
    def _2: ValDef
    def _3: List[Tree]
  }

  abstract class ModuleDefDeconstructor extends DeconstructorCommon[ModuleDef]{
    def _1: Modifiers
    def _2: Name
    def _3: Tree
  }

  abstract class DefDefDeconstructor extends DeconstructorCommon[DefDef]{
    def _1: Modifiers
    def _2: Name
    def _3: List[TypeDef]
    def _4: List[List[ValDef]]
    def _5: Tree
    def _6: Tree
  }

  abstract class ClosureDeconstructor extends DeconstructorCommon[Closure]{
    def _1: List[Tree] // environment
    def _2: Tree // meth
    def _3: Symbol // functionalInterface
  }

  abstract class ThisDeconstructor extends Deconstructor1Common[This, Name]{
    def apply(s: Symbol): Tree
  }

  abstract class IdentDeconstructor extends Deconstructor1Common[Ident, Name]{
  }

  abstract class LabeledDeconstructor extends DeconstructorCommon[Labeled]{
    def _1: Bind // bind
    def _2: Tree // expr
  }

  abstract class ReturnDeconstructor extends DeconstructorCommon[Return]{
    def _1: Tree // expr
    def _2: Symbol // target label, NoSymbol if return to method
  }

  abstract class WhileDoDeconstructor extends DeconstructorCommon[WhileDo]{
    def _1: Tree // cond
    def _2: Tree // body
  }

  abstract class ThrownException {
    def unapply(a: Annotation): Option[Symbol]
  }

  abstract class ThrowDeconstructor extends Deconstructor1Common[Throw, Tree]{
  }

  abstract class ConstantDeconstructor extends Deconstructor1Common[Constant, Any]{
  }

  abstract class NewDeconstructor extends Deconstructor1Common[New, Type]{
  }

  abstract class AlternativeDeconstructor extends Deconstructor1Common[Alternative, List[Tree]]{
  }

  abstract class BlockDeconstructor extends DeconstructorCommon[Block]{
    def _1: List[Tree]
    def _2: Tree
  }

  abstract class CaseDeconstructor extends DeconstructorCommon[CaseDef]{
    def _1: Tree
    def _2: Tree
    def _3: Tree
  }

  abstract class MatchDeconstructor extends DeconstructorCommon[Match]{
    def _1: Tree
    def _2: List[Tree]
  }

  abstract class LiteralDeconstructor extends Deconstructor1Common[Literal, Constant]{
  }

  abstract class AssignDeconstructor extends DeconstructorCommon[Assign]{
    def _1: Tree
    def _2: Tree
  }

  abstract class SelectDeconstructor extends DeconstructorCommon[Select]{
    def _1: Tree
    def _2: Name
  }

  abstract class ApplyDeconstructor extends DeconstructorCommon[Apply] {
    def _1: Tree
    def _2: List[Tree]
  }

  abstract class IfDeconstructor extends DeconstructorCommon[If]{
    def _1: Tree
    def _2: Tree
    def _3: Tree
  }

  abstract class ValDefDeconstructor extends DeconstructorCommon[ValDef]{
    def _1: Modifiers
    def _2: Name
    def _3: Tree
    def _4: Tree
  }


  abstract class ApplyDynamicDeconstructor extends DeconstructorCommon[ApplyDynamic]{
    def _1: Tree
    def _2: List[Tree]
  }


  abstract class TryDeconstructor extends DeconstructorCommon[Try]{
    def _1: Tree
    def _2: List[Tree]
    def _3: Tree
  }

  abstract class LabelDeconstructor extends DeconstructorCommon[LabelDef]{
    def _1: Name
    def _2: List[Symbol]
    def _3: Tree
  }

  abstract class TypedDeconstrutor extends DeconstructorCommon[Typed]{
    def _1: Tree
    def _2: Tree
  }

  abstract class SuperDeconstructor extends DeconstructorCommon[Super]{
    def _1: Tree
    def _2: Name
  }

  abstract class ArrayValueDeconstructor extends DeconstructorCommon[ArrayValue]{
    def _1: Type
    def _2: List[Tree]
  }

  abstract class TypeApplyDeconstructor extends DeconstructorCommon[TypeApply]{
    def _1: Tree
    def _2: List[Tree]
  }

  abstract class PositionHelper {
    def isDefined: Boolean
    def finalPosition: Position
    def line: Int
  }

  abstract class ConstantHelper {
    def tag: ConstantTag
    def longValue: Long
    def doubleValue: Double
    def charValue: Char
    def stringValue: String
    def byteValue: Byte
    def booleanValue: Boolean
    def shortValue: Short
    def intValue: Int
    def value: Any
    def floatValue: Float
    def typeValue: Type
    def symbolValue: Symbol
  }

  abstract class TreeHelper{
    def symbol: Symbol
    def tpe: Type
    def isEmpty: Boolean
    def pos: Position
    def exists(pred: Tree => Boolean): Boolean
  }

  abstract class SymbolHelper {
    // names
    def fullName(sep: Char): String
    def fullName: String
    def simpleName: Name
    def javaSimpleName: String
    def javaBinaryName: String
    def javaClassName: String
    def name: Name
    def rawname: String

    // types
    def info: Type
    def tpe: Type // todo whats the differentce between tpe and info?
    def thisType: Type

    /** Does this symbol actually correspond to an interface that will be emitted?
     *  In the backend, this should be preferred over `isInterface` because it
     *  also returns true for the symbols of the fake companion objects we
     *  create for Java-defined classes.
     */
    final def isEmittedInterface: Boolean = isInterface ||
      isJavaDefined && isModuleClass && companionClass.isInterface

    // tests
    def isClass: Boolean
    def isType: Boolean
    def isAnonymousClass: Boolean
    def isConstructor: Boolean
    def isExpanded: Boolean
    def isAnonymousFunction: Boolean
    def isMethod: Boolean
    def isPublic: Boolean
    def isSynthetic: Boolean
    def isPackageClass: Boolean
    def isModuleClass: Boolean
    def isModule: Boolean
    def isStrictFP: Boolean
    def isLabel: Boolean
    def hasPackageFlag: Boolean
    def isInterface: Boolean
    def isGetter: Boolean
    def isSetter: Boolean
    def isGetClass: Boolean
    def isJavaDefined: Boolean
    def isDeferred: Boolean
    def isPrivate: Boolean
    def getsJavaPrivateFlag: Boolean
    def isFinal: Boolean
    def getsJavaFinalFlag: Boolean
    def isStaticMember: Boolean
    def isBottomClass: Boolean
    def isBridge: Boolean
    def isArtifact: Boolean
    def hasEnumFlag: Boolean
    def hasAccessBoundary: Boolean
    def isVarargsMethod: Boolean
    def isDeprecated: Boolean
    def isMutable: Boolean
    def hasAbstractFlag: Boolean
    def hasModuleFlag: Boolean
    def isSynchronized: Boolean
    def isNonBottomSubClass(sym: Symbol): Boolean
    def hasAnnotation(sym: Symbol): Boolean
    def shouldEmitForwarders: Boolean
    def isJavaEntryPoint: Boolean
    def isJavaDefaultMethod: Boolean
    def isClassConstructor: Boolean
    def isSerializable: Boolean
    def isJavaEnum: Boolean

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean

    def isStaticConstructor: Boolean


    // navigation
    def owner: Symbol
    def rawowner: Symbol // todo ???
    def originalOwner: Symbol
    def parentSymbols: List[Symbol]
    def superClass: Symbol
    def enclClass: Symbol
    def linkedClassOfClass: Symbol
    def linkedClass: Symbol
    def companionClass: Symbol
    def companionModule: Symbol
    def companionSymbol: Symbol
    def moduleClass: Symbol
    def enclosingClassSym: Symbol
    def originalLexicallyEnclosingClass: Symbol
    def nextOverriddenSymbol: Symbol


    // members
    def primaryConstructor: Symbol
    def nestedClasses: List[Symbol]
    def memberClasses: List[Symbol]
    def annotations: List[Annotation]
    def companionModuleMembers: List[Symbol]
    def fieldSymbols: List[Symbol]
    def methodSymbols: List[Symbol]
    def serialVUID: Option[Long]


    def freshLocal(cunit: CompilationUnit, name: String, tpe: Type, pos: Position, flags: Flags): Symbol

    def getter(clz: Symbol): Symbol
    def setter(clz: Symbol): Symbol

    def moduleSuffix: String
    def outputDirectory: AbstractFile
    def pos: Position

    def throwsAnnotations: List[Symbol]

    /**
     * All interfaces implemented by a class, except for those inherited through the superclass.
     *
     */
    def superInterfaces: List[Symbol]

    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean

    /**
     * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
     *
     * The problem is that we are interested in a source-level property. Various phases changed the
     * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
     * Therefore, `sym.isStatic` is not what we want. For example, in
     *   object T { def f { object U } }
     * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
     */
    def isOriginallyStaticOwner: Boolean =
      isPackageClass || isModuleClass && originalOwner.originalLexicallyEnclosingClass.isOriginallyStaticOwner

    def samMethod(): Symbol

    /** Is this the symbol of one of the `scala.Function*` class ?
     *  Note that this will return false for subclasses of these classes.
     */
    def isFunctionClass: Boolean
  }

  abstract class TypeHelper {
    def <:<(other: Type): Boolean
    def =:=(other: Type): Boolean
    def paramTypes: List[Type]
    def params: List[Symbol]
    def resultType: Type
    def memberInfo(s: Symbol): Type
    def membersBasedOnFlags(excludedFlags: Flags, requiredFlags: Flags): List[Symbol]
    def members: List[Symbol]
    def decls: List[Symbol]
    def underlying: Type
    def parents: List[Type]
    def summaryString: String
    def typeSymbol: Symbol
    def member(string: Name): Symbol
    /**
     * This method returns the BType for a type reference, for example a parameter type.
     *
     * If the result is a ClassBType for a nested class, it is added to the innerClassBufferASM.
     *
     * If `t` references a class, toTypeKind ensures that the class is not an implementation class.
     * See also comment on getClassBTypeAndRegisterInnerClass, which is invoked for implementation
     * classes.
     */
    def toTypeKind(ctx: BCodeHelpers)(storage: ctx.BCInnerClassGen): ctx.bTypes.BType

    def isFinalType: Boolean
  }

  abstract class Primitives {
    def getPrimitive(app: Apply, reciever: Type): Int
    def isPrimitive(fun: Tree): Boolean
    def getPrimitive(sym: Symbol): Int
  }

  abstract class NameHelper {
    def isTypeName: Boolean
    def isTermName: Boolean
    def startsWith(s: String): Boolean
    def mangledString: String
  }

  abstract class AnnotationHelper{
    def atp: Type
    def symbol: Symbol
    def args: List[Tree]
    def assocs: List[(Name, /* ClassfileAnnotArg*/ Object)]
  }

  def debuglog(msg: => String): Unit
  def log(msg: => String): Unit
  def error(pos: Position, msg: String): Unit // reporter.error
  def warning(pos: Position, msg: String): Unit // reporter.warning
  def abort(msg: String): Nothing

  val ExcludedForwarderFlags: Flags
  val Flag_METHOD: Flags
  val Flag_SYNTHETIC: Flags

  abstract class Caches {
    def recordCache[T <: Clearable](cache: T): T
    def newWeakMap[K, V](): collection.mutable.WeakHashMap[K, V]
    def newMap[K, V](): collection.mutable.HashMap[K, V]
    def newSet[K](): collection.mutable.Set[K]
    def newWeakSet[K >: Null <: AnyRef](): dotty.tools.dotc.util.WeakHashSet[K]
    def newAnyRefMap[K <: AnyRef, V](): collection.mutable.AnyRefMap[K, V]
  }

  def perRunCaches: Caches

  def MODULE_INSTANCE_FIELD: String

  def dropModule(str: String): String

/*  Returns a ScalaSignature annotation if it must be added to this class, none otherwise.
 *  This annotation must be added to the class' annotations list when generating them.
 *
 *  Depending on whether the returned option is defined, it adds to `jclass` one of:
 *    (a) the ScalaSig marker attribute
 *        (indicating that a scala-signature-annotation aka pickle is present in this class); or
 *    (b) the Scala marker attribute
 *        (indicating that a scala-signature-annotation aka pickle is to be found in another file).
 *
 *
 *  @param jclassName The class file that is being readied.
 *  @param sym    The symbol for which the signature has been entered in the symData map.
 *                This is different than the symbol
 *                that is being generated in the case of a mirror class.
 *  @return       An option that is:
 *                - defined and contains an AnnotationInfo of the ScalaSignature type,
 *                  instantiated with the pickle signature for sym.
 *                - empty if the jclass/sym pair must not contain a pickle.
 *
 *  must-single-thread
 */
  def getAnnotPickle(jclassName: String, sym: Symbol): Option[Annotation]
}

abstract class BackendInterfaceDefinitions { self: BackendInterface =>
  val nme_valueOf: Name

  /* magic instances */
  val NoPosition: Position
  val NoSymbol: Symbol
  val EmptyTree: Tree
  val NothingClass: Symbol
  val NullClass: Symbol
  val ObjectClass: Symbol
  val Object_isInstanceOf: Symbol
  val Object_asInstanceOf: Symbol
  val Object_synchronized: Symbol
  val Object_equals: Symbol
  val ArrayClass: Symbol

  val UnitClass: Symbol
  val BooleanClass: Symbol
  val CharClass: Symbol
  val ShortClass: Symbol
  val ClassClass: Symbol
  val ByteClass: Symbol
  val IntClass: Symbol
  val LongClass: Symbol
  val FloatClass: Symbol
  val DoubleClass: Symbol

  // Class symbols used in backend.
  // Vals becouse they are to frequent in scala programs so that they are already loaded by backend

  lazy val NativeAttr: Symbol = requiredClass[scala.native]
  lazy val TransientAttr = requiredClass[scala.transient]
  lazy val VolatileAttr = requiredClass[scala.volatile]
  lazy val LambdaMetaFactory = getClassIfDefined("java.lang.invoke.LambdaMetafactory")
  lazy val MethodHandle = getClassIfDefined("java.lang.invoke.MethodHandle")

  val ScalaATTRName: String = "Scala"
  val ScalaSignatureATTRName: String = "ScalaSig"

  def doLabmdasFollowJVMMetafactoryOrder: Boolean = true

  val BoxedBooleanClass: Symbol = requiredClass[java.lang.Boolean]
  val BoxedByteClass: Symbol = requiredClass[java.lang.Byte]
  val BoxedShortClass: Symbol = requiredClass[java.lang.Short]
  val BoxedCharacterClass: Symbol = requiredClass[java.lang.Character]
  val BoxedIntClass: Symbol  = requiredClass[java.lang.Integer]
  val BoxedLongClass: Symbol = requiredClass[java.lang.Long]
  val BoxedFloatClass: Symbol = requiredClass[java.lang.Float]
  val BoxedDoubleClass: Symbol = requiredClass[java.lang.Double]
  val StringClass: Symbol = requiredClass[java.lang.String]
  val JavaStringBuilderClass: Symbol = requiredClass[java.lang.StringBuilder]
  val JavaStringBufferClass: Symbol = requiredClass[java.lang.StringBuffer]
  val JavaCharSequenceClass: Symbol = requiredClass[java.lang.CharSequence]
  val ThrowableClass: Symbol = requiredClass[java.lang.Throwable]
  val JavaCloneableClass: Symbol = requiredClass[java.lang.Cloneable]
  val NullPointerExceptionClass: Symbol  = requiredClass[java.lang.NullPointerException]
  val JavaSerializableClass: Symbol = requiredClass[java.io.Serializable]
  val SerializableClass: Symbol = requiredClass[scala.Serializable]
  val ClassCastExceptionClass: Symbol = requiredClass[java.lang.ClassCastException]
  val IllegalArgExceptionClass: Symbol = requiredClass[java.lang.IllegalArgumentException]
  val SerializedLambdaClass: Symbol = requiredClass[java.lang.invoke.SerializedLambda]

  val ClassfileAnnotationClass: Symbol = requiredClass[scala.annotation.ClassfileAnnotation]
  val BoxedNumberClass: Symbol = requiredClass[java.lang.Number]
  val ThrowsClass: Symbol = requiredClass[scala.throws[_]]

  // Module symbols used in backend
  val StringModule: Symbol = StringClass.linkedClassOfClass
  val ScalaRunTimeModule: Symbol = requiredModule[scala.runtime.ScalaRunTime.type]


  // types used in backend

  val Object_Type: Type
  val Throwable_Type: Type
  // methods used in backend

  def isArrayClone(fun: Tree): Boolean
  val hashMethodSym: Symbol
  val externalEqualsNumNum: Symbol
  val externalEqualsNumChar: Symbol
  val externalEqualsNumObject: Symbol
  val externalEquals: Symbol

  val MaxFunctionArity: Int
  val FunctionClass: Array[Symbol]
  val AbstractFunctionClass: Array[Symbol]
  val PartialFunctionClass: Symbol
  val AbstractPartialFunctionClass: Symbol

  /* The Object => String overload. */
  val String_valueOf: Symbol

  def isNull(t: Tree): Boolean = t match {
    case Literal(Constant(null)) => true
    case _ => false
  }
  def isLiteral(t: Tree): Boolean = t match {
    case Literal(_) => true
    case _ => false
  }
  def isNonNullExpr(t: Tree): Boolean = isLiteral(t) || ((t.symbol ne null) && t.symbol.isModule)
  def ifOneIsNull(l: Tree, r: Tree): Tree = if (isNull(l)) r else if (isNull(r)) l else null

  private val primitiveCompilationUnits = Set(
    "Unit.scala",
    "Boolean.scala",
    "Char.scala",
    "Byte.scala",
    "Short.scala",
    "Int.scala",
    "Float.scala",
    "Long.scala",
    "Double.scala"
  )

  def currentUnit: CompilationUnit

  /**
   * True if the current compilation unit is of a primitive class (scala.Boolean et al).
   * Used only in assertions.
   */
  def isCompilingPrimitive = {
    primitiveCompilationUnits(sourceFileFor(currentUnit))
  }

  def isCompilingArray = {
    sourceFileFor(currentUnit) == "Array.scala"
  }
}
