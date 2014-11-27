package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc
import dotty.tools.dotc.backend.jvm.DottyPrimitives
import dotty.tools.dotc.transform.Erasure

import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.internal.util.WeakHashSet
import scala.reflect.io.AbstractFile
import scala.tools.asm.{ClassVisitor, FieldVisitor, MethodVisitor}
import scala.tools.nsc.backend.jvm.{BCodeHelpers, BackendInterface}
import dotty.tools.dotc.core._
import Periods._
import SymDenotations._
import Contexts._
import Types._
import Symbols._
import Denotations._
import Phases._
import java.lang.AssertionError
import dotty.tools.dotc.util.{Positions, DotClass}
import Decorators._
import tpd._
import StdNames.nme

abstract class DottyBackendInterface()(implicit ctx: Context) extends BackendInterface{
  trait NonExistentTree extends tpd.Tree
  type Symbol          = Symbols.Symbol
  type Type            = Types.Type
  type Tree            = tpd.Tree
  type CompilationUnit = dotc.CompilationUnit
  type Constant        = Constants.Constant
  type Literal         = tpd.Literal
  type Position        = Positions.Position
  type Name            = Names.Name
  type ClassDef        = tpd.TypeDef
  type TypeDef         = tpd.TypeDef
  type Apply           = tpd.Apply
  type TypeApply       = tpd.TypeApply
  type Try             = tpd.Try
  type Assign          = tpd.Assign
  type Ident           = tpd.Ident
  type If              = tpd.If
  type ValDef          = tpd.ValDef
  type Throw           = tpd.Throw
  type Return          = tpd.Return
  type Block           = tpd.Block
  type Typed           = tpd.Typed
  type Match           = tpd.Match
  type This            = tpd.This
  type CaseDef         = tpd.CaseDef
  type Alternative     = tpd.Alternative
  type DefDef          = tpd.DefDef
  type Template        = tpd.Template
  type Select          = tpd.Select
  type Bind            = tpd.Bind
  type New             = tpd.New
  type Super           = tpd.Super
  type Modifiers       = tpd.Modifiers
  type Annotation      = NonExistentTree
  type ArrayValue      = NonExistentTree
  type ApplyDynamic    = NonExistentTree
  type ModuleDef       = NonExistentTree
  type LabelDef        = NonExistentTree


  val NoSymbol = Symbols.NoSymbol
  val NoPosition: Position = Positions.NoPosition
  val EmptyTree: Tree = tpd.EmptyTree


  val UnitTag: ConstantTag = Constants.UnitTag
  val IntTag: ConstantTag = Constants.IntTag
  val FloatTag: ConstantTag = Constants.FloatTag
  val NullTag: ConstantTag = Constants.NullTag
  val BooleanTag: ConstantTag = Constants.BooleanTag
  val ByteTag: ConstantTag = Constants.ByteTag
  val ShortTag: ConstantTag = Constants.ShortTag
  val CharTag: ConstantTag = Constants.CharTag
  val DoubleTag: ConstantTag = Constants.DoubleTag
  val LongTag: ConstantTag = Constants.LongTag
  val StringTag: ConstantTag = Constants.StringTag
  val ClazzTag: ConstantTag = Constants.ClazzTag
  val EnumTag: ConstantTag = Constants.EnumTag

  val nme_This: Name = StdNames.nme.This
  val nme_EMPTY_PACKAGE_NAME: Name = StdNames.nme.EMPTY_PACKAGE
  val nme_CONSTRUCTOR: Name = StdNames.nme.CONSTRUCTOR
  val nme_WILDCARD: Name = StdNames.nme.WILDCARD
  val nme_THIS: Name = StdNames.nme.THIS
  val nme_PACKAGE: Name = StdNames.nme.PACKAGE
  val nme_EQEQ_LOCAL_VAR: Name = StdNames.nme.EQEQ_LOCAL_VAR

  val BoxesRunTimeModule     = ctx.requiredModule("scala.runtime.BoxesRunTime")
  val BoxesRunTimeClass      = toDenot(BoxesRunTimeModule).moduleClass.asClass

  val nme_valueOf: Name = StdNames.nme.valueOf
  val NothingClass: Symbol = defn.NothingClass
  val NullClass: Symbol = defn.NullClass
  val ObjectClass: Symbol = defn.ObjectClass
  val Object_isInstanceOf: Symbol = defn.Any_isInstanceOf
  val Object_asInstanceOf: Symbol = defn.Any_asInstanceOf
  val Object_equals: Symbol = defn.Any_equals
  val ArrayClass: Symbol = defn.ArrayClass
  val UnitClass: Symbol = defn.UnitClass
  val BooleanClass: Symbol = defn.BooleanClass
  val CharClass: Symbol = defn.CharClass
  val ShortClass: Symbol = defn.ShortClass
  val ClassClass: Symbol = defn.ClassClass
  val ByteClass: Symbol = defn.ByteClass
  val IntClass: Symbol = defn.IntClass
  val LongClass: Symbol = defn.LongClass
  val FloatClass: Symbol = defn.FloatClass
  val DoubleClass: Symbol = defn.DoubleClass
  val Array_clone: Symbol = defn.Array_clone
  val hashMethodSym: Symbol = ctx.requiredMethod(ScalaRunTimeModule.asClass, StdNames.nme.hash_)
  val externalEqualsNumNum: Symbol = ctx.requiredMethod(BoxesRunTimeClass, nme.equalsNumNum)
  val externalEqualsNumChar: Symbol = ctx.requiredMethod(BoxesRunTimeClass, nme.equalsNumChar)
  val externalEqualsNumObject: Symbol = ctx.requiredMethod(BoxesRunTimeClass, nme.equalsNumObject)
  val externalEquals: Symbol = ctx.requiredMethod(BoxesRunTimeClass, nme.equals_)
  val MaxFunctionArity: Int = Definitions.MaxFunctionArity
  val FunctionClass: Array[Symbol] = defn.FunctionClass.asInstanceOf[Array[Symbol]]
  val AbstractFunctionClass: Array[Symbol] = defn.AbstractFunctionClass.asInstanceOf[Array[Symbol]]
  val PartialFunctionClass: Symbol = defn.PartialFunctionClass
  val AbstractPartialFunctionClass: Symbol = defn.AbstractPartialFunctionClass
  val String_valueOf: Symbol = defn.String_valueOf_Object

  def boxMethods: Map[Symbol, Symbol] = defn.ScalaBoxedClasses.map(x => (x, Erasure.Boxing.boxMethod(x.asClass))).toMap
  def unboxMethods: Map[Symbol, Symbol] = defn.ScalaBoxedClasses.map(x => (x, Erasure.Boxing.unboxMethod(x.asClass))).toMap
  def isBox(sym: Symbol): Boolean = Erasure.Boxing.isBox(sym)
  def isUnbox(sym: Symbol): Boolean = Erasure.Boxing.isUnbox(sym)

  val primitives: Primitives = new Primitives {
    val primitives = new DottyPrimitives(ctx, genBcode)
    def getPrimitive(methodSym: Symbol, reciever: Type): Int = primitives.getPrimitive(methodSym, reciever)

    def getPrimitive(sym: Symbol): Int = primitives.getPrimitive(sym)

    def isPrimitive(sym: Symbol): Boolean = primitives.isPrimitive(sym)
  }
  implicit val TypeDefTag: ClassTag[TypeDef] = ClassTag[TypeDef](classOf[TypeDef])
  implicit val ApplyTag: ClassTag[Apply] = ClassTag[Apply](classOf[Apply])
  implicit val SelectTag: ClassTag[Select] = ClassTag[Select](classOf[Select])
  implicit val TypeApplyTag: ClassTag[TypeApply] = ClassTag[TypeApply](classOf[TypeApply])
  implicit val ClassDefTag: ClassTag[ClassDef] = ClassTag[TypeDef](classOf[TypeDef])
  implicit val TryTag: ClassTag[Try] = ClassTag[Try](classOf[Try])
  implicit val AssignTag: ClassTag[Assign] = ClassTag[Assign](classOf[Assign])
  implicit val IdentTag: ClassTag[Ident] = ClassTag[Ident](classOf[Ident])
  implicit val IfTag: ClassTag[If] = ClassTag[If](classOf[If])
  implicit val LabelDefTag: ClassTag[LabelDef] = ClassTag[LabelDef](classOf[LabelDef])
  implicit val ValDefTag: ClassTag[ValDef] = ClassTag[ValDef](classOf[ValDef])
  implicit val ThrowTag: ClassTag[Throw] = ClassTag[Throw](classOf[Throw])
  implicit val ReturnTag: ClassTag[Return] = ClassTag[Return](classOf[Return])
  implicit val LiteralTag: ClassTag[Literal] = ClassTag[Literal](classOf[Literal])
  implicit val BlockTag: ClassTag[Block] = ClassTag[Block](classOf[Block])
  implicit val TypedTag: ClassTag[Typed] = ClassTag[Typed](classOf[Typed])
  implicit val ArrayValueTag: ClassTag[ArrayValue] = ClassTag[ArrayValue](classOf[ArrayValue])
  implicit val MatchTag: ClassTag[Match] = ClassTag[Match](classOf[Match])
  implicit val CaseDefTag: ClassTag[CaseDef] = ClassTag[CaseDef](classOf[CaseDef])
  implicit val ThisTag: ClassTag[This] = ClassTag[This](classOf[This])
  implicit val AlternativeTag: ClassTag[Alternative] = ClassTag[Alternative](classOf[Alternative])
  implicit val DefDefTag: ClassTag[DefDef] = ClassTag[DefDef](classOf[DefDef])
  implicit val ModuleDefTag: ClassTag[ModuleDef] = ClassTag[ModuleDef](classOf[ModuleDef])
  implicit val NameTag: ClassTag[Name] = ClassTag[Name](classOf[Name])
  implicit val TemplateTag: ClassTag[Template] = ClassTag[Template](classOf[Template])
  implicit val BindTag: ClassTag[Bind] = ClassTag[Bind](classOf[Bind])
  implicit val NewTag: ClassTag[New] = ClassTag[New](classOf[New])
  implicit val ApplyDynamicTag: ClassTag[ApplyDynamic] = ClassTag[ApplyDynamic](classOf[ApplyDynamic])
  implicit val SuperTag: ClassTag[Super] = ClassTag[Super](classOf[Super])
  implicit val ConstantClassTag: ClassTag[Constant] = ClassTag[Constant](classOf[Constant])

  /* dont emit any annotations for now*/
  def isRuntimeVisible(annot: Annotation): Boolean = false
  def shouldEmitAnnotation(annot: Annotation): Boolean = false

  def emitAnnotations(cw: ClassVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit = ()
  def emitAnnotations(mw: MethodVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit = ()
  def emitAnnotations(fw: FieldVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit = ()
  def emitParamAnnotations(jmethod: MethodVisitor, pannotss: List[List[Annotation]], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit = ()
  def getAnnotPickle(jclassName: String, sym: Symbol): Option[Annotation] = None


  def getRequiredClass(fullname: String): Symbol = ctx.requiredClass(fullname.toTermName)

  def getClassIfDefined(fullname: String): Symbol = NoSymbol // used only for android. todo: implement

  private def erasureString(clazz: Class[_]): String = {
    if (clazz.isArray) "Array[" + erasureString(clazz.getComponentType) + "]"
    else clazz.getName
  }

  def requiredClass[T](implicit evidence: ClassTag[T]): Symbol = {
    ctx.requiredClass(erasureString(evidence.runtimeClass).toTermName)
  }

  def requiredModule[T](implicit evidence: ClassTag[T]): Symbol = {
    ctx.requiredModule(erasureString(evidence.runtimeClass).toTermName)
  }


  def debuglog(msg: => String): Unit = ctx.debuglog(msg)
  def informProgress(msg: String): Unit = ctx.informProgress(msg)
  def log(msg: => String): Unit = ctx.log(msg)
  def error(pos: Position, msg: String): Unit = ctx.error(msg, pos)
  def warning(pos: Position, msg: String): Unit = ctx.warning(msg, pos)
  def abort(msg: String): Nothing = {
    ctx.error(msg)
    ???
  }

  def emitAsmp: Option[String] = None

  def dumpClasses: Option[String] =
    if(ctx.settings.Ydumpclasses.isDefault) None
    else Some(ctx.settings.Ydumpclasses.value)

  def mainClass: Option[String] =
    if (ctx.settings.mainClass.isDefault) None
    else Some(ctx.settings.mainClass.value)
  def setMainClass(name: String): Unit = ctx.settings.mainClass.update(name)


  def noForwarders: Boolean = ctx.settings.noForwarders.value
  def debuglevel: Int = 0
  def settings_debug: Boolean = ctx.settings.debug.value
  def targetPlatform: String = ctx.settings.target.value

  val perRunCaches: Caches = new Caches {
    def newAnyRefMap[K <: AnyRef, V](): mutable.AnyRefMap[K, V] = new mutable.AnyRefMap[K, V]()
    def newWeakMap[K, V](): mutable.WeakHashMap[K, V] = new mutable.WeakHashMap[K, V]()
    def recordCache[T <: Clearable](cache: T): T = cache
    def newWeakSet[K <: AnyRef](): WeakHashSet[K] = new WeakHashSet[K]()
    def newMap[K, V](): mutable.HashMap[K, V] = new mutable.HashMap[K, V]()
    def newSet[K](): mutable.Set[K] = new mutable.HashSet[K]
  }



  val MODULE_INSTANCE_FIELD: String = nme.MODULE_INSTANCE_FIELD.toString

  def internalNameString(offset: Int, length: Int): String = new String(Names.chrs, offset, length)

  def newTermName(prefix: String): Name = prefix.toTermName

  val Flag_SYNTHETIC: Flags = Flags.Synthetic.bits
  val Flag_METHOD: Flags = Flags.Method.bits
  val ExcludedForwarderFlags: Flags = {
      Flags.Specialized | Flags.Lifted | Flags.Protected | Flags.JavaStatic |
     Flags.ExpandedName | Flags.Bridge | Flags.VBridge | Flags.Private | Flags.Macro
  }.bits


  def isQualifierSafeToElide(qual: Tree): Boolean = false // todo: implement
  def getLabelDefOwners(t: Tree): Map[Tree, List[LabelDef]] = Map.empty

  // todo: remove
  def isMaybeBoxed(sym: Symbol) = {
    (sym == ObjectClass) ||
      (sym == JavaSerializableClass) ||
      (sym == defn.ComparableClass) ||
      (sym derivesFrom BoxedNumberClass) ||
      (sym derivesFrom BoxedCharacterClass) ||
      (sym derivesFrom BoxedBooleanClass)
  }

  def getSingleOutput: Option[AbstractFile] = None // todo: implement


  def getGenericSignature(sym: Symbol, owner: Symbol): String = "" // todo: implement

  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String = "" // todo: implement


  def sourceFileFor(cu: CompilationUnit): String = cu.source.file.name



  implicit def positionHelper(a: Position): PositionHelper = new PositionHelper {
    def isDefined: Boolean = a.exists
    def line: Int = sourcePos(a).line
    def finalPosition: Position = a
  }

  implicit def constantHelper(a: Constant): ConstantHelper = new ConstantHelper {
    def booleanValue: Boolean = a.booleanValue
    def longValue: Long = a.longValue
    def byteValue: Byte = a.byteValue
    def stringValue: String = a.stringValue
    def symbolValue: Symbol = a.symbolValue
    def floatValue: Float = a.floatValue
    def value: Any = a.value
    def tag: ConstantTag = a.tag
    def typeValue: Type = a.typeValue
    def shortValue: Short = a.shortValue
    def intValue: Int = a.intValue
    def doubleValue: Double = a.doubleValue
    def charValue: Char = a.charValue
  }


  implicit def treeHelper(a: Tree): TreeHelper = new TreeHelper {
    def symbol: Symbol = a.symbol

    def pos: Position = a.pos

    def isEmpty: Boolean = a.isEmpty

    def tpe: Type = a.tpe

    def exists(pred: (Tree) => Boolean): Boolean = a.find(pred).isDefined
  }


  implicit def annotHelper(a: Annotation): AnnotationHelper = new AnnotationHelper {
    def atp: Type = a.tree.tpe

    def assocs: List[(Name, Object)] = ???

    def symbol: Symbol = a.tree.symbol

    def args: List[Tree] = ???
  }


  implicit def nameHelper(n: Name): NameHelper = new NameHelper {
    def toTypeName: Name = n.toTypeName
    def isTypeName: Boolean = n.isTypeName
    def toTermName: Name = n.toTermName
    def dropModule: Name = ???

    def len: Int = n.length
    def offset: Int = n.start
    def isTermName: Boolean = n.isTermName
    def startsWith(s: String): Boolean = n.startsWith(s)
  }


  implicit def symHelper(sym: Symbol): SymbolHelper = new SymbolHelper {
    // names
    def fullName(sep: Char): String = sym.showFullName
    def fullName: String = sym.showFullName
    def simpleName: Name = sym.name
    def javaSimpleName: Name = toDenot(sym).fullName // addModuleSuffix(simpleName.dropLocal)
    def javaBinaryName: Name = toDenot(sym).fullNameSeparated('/') // addModuleSuffix(fullNameInternal('/'))
    def javaClassName: String = toDenot(sym).fullName.toString// addModuleSuffix(fullNameInternal('.')).toString
    def name: Name = sym.name
    def rawname: Name = sym.name // todo ????

    // types
    def info: Type = toDenot(sym).info
    def tpe: Type = toDenot(sym).info // todo whats the differentce between tpe and info?
    def thisType: Type = toDenot(sym).thisType

    // tests
    def isClass: Boolean = sym.isClass
    def isType: Boolean = sym.isType
    def isAnonymousClass: Boolean = toDenot(sym).isAnonymousClass
    def isConstructor: Boolean = toDenot(sym).isConstructor
    def isAnonymousFunction: Boolean = toDenot(sym).isAnonymousFunction
    def isMethod: Boolean = sym is Flags.Method
    def isPublic: Boolean =  sym.flags.is(Flags.EmptyFlags, Flags.Private | Flags.Protected)
    def isSynthetic: Boolean = sym is Flags.Synthetic
    def isPackageClass: Boolean = sym is Flags.PackageClass
    def isModuleClass: Boolean = sym is Flags.ModuleClass
    def isModule: Boolean = sym is Flags.Module
    def isStrictFP: Boolean = false // todo: implement
    def isLabel: Boolean = sym is Flags.Label
    def hasPackageFlag: Boolean = sym is Flags.Package
    def isImplClass: Boolean = sym is Flags.ImplClass
    def isInterface: Boolean = (sym is Flags.JavaInterface) || (sym is Flags.PureInterface)
    def hasGetter: Boolean = false // used only for generaration of beaninfo todo: implement
    def isGetter: Boolean = toDenot(sym).isGetter
    def isSetter: Boolean = toDenot(sym).isSetter
    def isGetClass: Boolean = sym eq defn.Any_getClass
    def isJavaDefined: Boolean = sym is Flags.JavaDefined
    def isDeferred: Boolean = sym is Flags.Deferred
    def isPrivate: Boolean = sym is Flags.Private
    def isFinal: Boolean = sym is Flags.Final
    def isStaticMember: Boolean = (sym is Flags.JavaStatic) || (owner is Flags.ImplClass)
    def isBottomClass: Boolean = (sym ne defn.NullClass) && (sym ne defn.NothingClass)
    def isBridge: Boolean = sym is Flags.Bridge
    def isArtifact: Boolean = sym is Flags.Artifact
    def hasEnumFlag: Boolean = sym is Flags.Enum
    def hasAccessBoundary: Boolean = sym.accessBoundary(defn.RootClass) ne defn.RootClass
    def isVarargsMethod: Boolean = sym is Flags.JavaVarargs
    def isDeprecated: Boolean = false
    def isMutable: Boolean = sym is Flags.Mutable
    def hasAbstractFlag: Boolean = sym is Flags.Abstract
    def hasModuleFlag: Boolean = sym is Flags.Module
    def isSynchronized: Boolean = sym is Flags.Synchronized
    def isNonBottomSubClass(other: Symbol): Boolean = sym.derivesFrom(other)
    def hasAnnotation(sym: Symbol): Boolean = false
    def shouldEmitForwarders: Boolean =  //exitingPickler { !(sym.name.toString contains '$')
      (sym is Flags.Module) && !(sym is Flags.ImplClass) /// !sym.isNestedClass
    def isJavaEntryPoint: Boolean = CollectEntryPoints.isJavaEntyPoint(sym)

    def isClassConstructor: Boolean = sym.name == nme.CONSTRUCTOR

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean

    def isStaticConstructor: Boolean = isStaticMember && isClassConstructor


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



    // members
    def primaryConstructor: Symbol
    def nestedClasses: List[Symbol]
    def memberClasses: List[Symbol]
    def annotations: List[Annotation]
    def companionModuleMembers: List[Symbol]
    def fieldSymbols: List[Symbol]
    def methodSymbols: List[Symbol]
    def serialVUID: Option[Long]


    def freshLocal(cunit: CompilationUnit, name: String, pos: Position, flags: Flags): Symbol

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
    def isOriginallyStaticOwner: Boolean


    def addRemoteRemoteExceptionAnnotation: Unit
  }

  val genBcode: GenBCode
}