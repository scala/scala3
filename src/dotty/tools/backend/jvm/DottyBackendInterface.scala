package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc
import dotty.tools.dotc.backend.jvm.DottyPrimitives
import dotty.tools.dotc.core.Flags.FlagSet
import dotty.tools.dotc.transform.Erasure
import dotty.tools.dotc.transform.SymUtils._
import java.io.{File => JFile}

import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.internal.util.WeakHashSet
import scala.reflect.io.{Directory, PlainDirectory, AbstractFile}
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

class DottyBackendInterface()(implicit ctx: Context) extends BackendInterface{
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
  type LabelDef        = tpd.DefDef
  type Closure         = tpd.Closure

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
  val nme_apply = StdNames.nme.apply
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
  val hashMethodSym: Symbol = NoSymbol // used to dispatch ## on primitives to ScalaRuntime.hash. Should be implemented by a miniphase
  val externalEqualsNumNum: Symbol = ctx.requiredMethod(BoxesRunTimeClass, nme.equalsNumNum)
  lazy val externalEqualsNumChar: Symbol = ??? // ctx.requiredMethod(BoxesRunTimeClass, nme.equalsNumChar) // this method is private
  val externalEqualsNumObject: Symbol = ctx.requiredMethod(BoxesRunTimeClass, nme.equalsNumObject)
  val externalEquals: Symbol = ctx.requiredMethod(BoxesRunTimeClass, nme.equals_)
  val MaxFunctionArity: Int = Definitions.MaxFunctionArity
  val FunctionClass: Array[Symbol] = defn.FunctionClass.asInstanceOf[Array[Symbol]]
  val AbstractFunctionClass: Array[Symbol] = defn.AbstractFunctionClass.asInstanceOf[Array[Symbol]]
  val PartialFunctionClass: Symbol = defn.PartialFunctionClass
  val AbstractPartialFunctionClass: Symbol = defn.AbstractPartialFunctionClass
  val String_valueOf: Symbol = defn.String_valueOf_Object

  def boxMethods: Map[Symbol, Symbol] = defn.ScalaValueClasses.map{x =>
    (x, Erasure.Boxing.boxMethod(x.asClass))
  }.toMap
  def unboxMethods: Map[Symbol, Symbol] = defn.ScalaValueClasses.map(x => (x, Erasure.Boxing.unboxMethod(x.asClass))).toMap

  private val mkArrayNames: Set[String] = Set("Byte", "Float", "Char", "Double", "Boolean", "Unit", "Long", "Int", "Short", "Ref")

  override lazy val syntheticArrayConstructors: Set[Symbol] = mkArrayNames.map(nm => ctx.requiredMethod(toDenot(defn.DottyArraysModule).moduleClass.asClass, s"new${nm}Array"))

  def isBox(sym: Symbol): Boolean = Erasure.Boxing.isBox(sym)
  def isUnbox(sym: Symbol): Boolean = Erasure.Boxing.isUnbox(sym)

  val primitives: Primitives = new Primitives {
    val primitives = new DottyPrimitives(ctx)
    def getPrimitive(app: Apply, reciever: Type): Int = primitives.getPrimitive(app, reciever)

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
  implicit val ClosureTag: ClassTag[Closure] = ClassTag[Closure](classOf[Closure])

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
    throw new RuntimeException(msg)
  }

  def emitAsmp: Option[String] = None

  def shouldEmitJumpAfterLabels = true

  def dumpClasses: Option[String] =
    if(ctx.settings.Ydumpclasses.isDefault) None
    else Some(ctx.settings.Ydumpclasses.value)

  def mainClass: Option[String] =
    if (ctx.settings.mainClass.isDefault) None
    else Some(ctx.settings.mainClass.value)
  def setMainClass(name: String): Unit = ctx.settings.mainClass.update(name)


  def noForwarders: Boolean = ctx.settings.noForwarders.value
  def debuglevel: Int = 3 // 0 -> no debug info; 1-> filename; 2-> lines; 3-> varnames
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
  def getLabelDefOwners(tree: Tree): Map[Tree, List[LabelDef]] = {
    // for each rhs of a defdef returns LabelDefs inside this DefDef
    val res = new collection.mutable.HashMap[Tree, List[LabelDef]]()

    val t = new TreeTraverser {
      var outerRhs: Tree = tree

      def traverse(tree: tpd.Tree): Unit = tree match {
        case t: DefDef =>
          if (t.symbol is Flags.Label)
            res.put(outerRhs, t :: res.getOrElse(outerRhs, Nil))
          else outerRhs = t
          traverseChildren(t)
        case _ => traverseChildren(tree)
      }
    }

    t.traverse(tree)
    res.toMap
  }

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


  def getGenericSignature(sym: Symbol, owner: Symbol): String = null // todo: implement

  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String = null // todo: implement


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
    def javaSimpleName: Name = toDenot(sym).name // addModuleSuffix(simpleName.dropLocal)
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
    def isStaticModuleClass: Boolean = sym.isStatic && (sym is Flags.Module)

    def isStaticConstructor: Boolean = isStaticMember && isClassConstructor


    // navigation
    def owner: Symbol = toDenot(sym).owner
    def rawowner: Symbol = owner
    def originalOwner: Symbol = toDenot(sym)(ctx.withPhase(ctx.typerPhase)).owner
    def parentSymbols: List[Symbol] = toDenot(sym).info.parents.map(_.typeSymbol)
    def superClass: Symbol = toDenot(sym).superClass
    def enclClass: Symbol = toDenot(sym).enclosingClass
    def linkedClassOfClass: Symbol = toDenot(sym).companionModule
    def linkedClass: Symbol = linkedClassOfClass //exitingPickler(sym.linkedClassOfClass)
    def companionClass: Symbol = toDenot(sym).companionClass
    def companionModule: Symbol = toDenot(sym).companionModule
    def companionSymbol: Symbol = if (sym is Flags.Module) companionClass else companionModule
    def moduleClass: Symbol = toDenot(sym).moduleClass
    def enclosingClassSym: Symbol = enclClass //todo is handled specially for JavaDefined symbols in scalac



    // members
    def primaryConstructor: Symbol = toDenot(sym).primaryConstructor
    def nestedClasses: List[Symbol] = memberClasses //exitingPhase(currentRun.lambdaliftPhase)(sym.memberClasses)
    def memberClasses: List[Symbol] = toDenot(sym).info.memberClasses.map(_.symbol).toList
    def annotations: List[Annotation] = Nil
    def companionModuleMembers: List[Symbol] =  {
      // phase travel to exitingPickler: this makes sure that memberClassesOf only sees member classes,
      // not local classes of the companion module (E in the exmaple) that were lifted by lambdalift.
      if (linkedClass.isTopLevelModuleClass) /*exitingPickler*/ linkedClass.memberClasses
      else Nil
    }
    def fieldSymbols: List[Symbol] = toDenot(sym).info.memberClasses.map(_.symbol).toList
    def methodSymbols: List[Symbol] =
      for (f <- toDenot(sym).info.decls.toList if !f.isMethod && f.isTerm && !f.isModule) yield f
    def serialVUID: Option[Long] = None


    def freshLocal(cunit: CompilationUnit, name: String, pos: Position, flags: Flags): Symbol = ???

    def getter(clz: Symbol): Symbol = decorateSymbol(sym).getter
    def setter(clz: Symbol): Symbol = decorateSymbol(sym).setter

    def moduleSuffix: String = "" // todo: validate that names already have $ suffix
    def outputDirectory: AbstractFile = new PlainDirectory(new Directory(new JFile(ctx.settings.d.value)))
    def pos: Position = sym.pos

    def throwsAnnotations: List[Symbol] = Nil

    /**
     * All interfaces implemented by a class, except for those inherited through the superclass.
     *
     */
    def superInterfaces: List[Symbol] = sym.mixins

    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean = sym.isModuleClass && sym.isStatic

    /**
     * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
     *
     * The problem is that we are interested in a source-level property. Various phases changed the
     * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
     * Therefore, `sym.isStatic` is not what we want. For example, in
     *   object T { def f { object U } }
     * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
     */
    def isOriginallyStaticOwner: Boolean = sym.isStatic


    def addRemoteRemoteExceptionAnnotation: Unit = ()
  }


  implicit def typeHelper(tp: Type): TypeHelper = new TypeHelper {
    def member(string: Name): Symbol = tp.member(string.toTermName).symbol

    def isFinalType: Boolean = tp.typeSymbol is Flags.Final //in scalac checks for type parameters. Why? Aren't they gone by backend?

    def underlying: Type = tp match {
      case t: TypeProxy => t.underlying
      case _ => tp
    }

    def paramTypes: List[Type] = tp.firstParamTypes

    def <:<(other: Type): Boolean = tp <:< other

    def memberInfo(s: Symbol): Type = tp.memberInfo(s)

    def decls: List[Symbol] = tp.decls.map(_.symbol).toList

    def members: List[Symbol] =
      tp.memberDenots(takeAllFilter, (name, buf) => buf ++= member(name).alternatives).map(_.symbol).toList

    def typeSymbol: Symbol = tp.typeSymbol

    def =:=(other: Type): Boolean = tp =:= other

    def membersBasedOnFlags(excludedFlags: Flags, requiredFlags: Flags): List[Symbol] =
      tp.membersBasedOnFlags(FlagSet(requiredFlags), FlagSet(excludedFlags)).map(_.symbol).toList

    def resultType: Type = tp.resultType

    def toTypeKind(ct: BCodeHelpers)(storage: ct.BCInnerClassGen): ct.bTypes.BType = {
      import ct.bTypes._
      val defn = ctx.definitions
      import coreBTypes._
      import Types._
      /**
       * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
       * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
       */
      def primitiveOrClassToBType(sym: Symbol): BType = {
        assert(sym.isClass, sym)
        assert(sym != ArrayClass || isCompilingArray, sym)
        primitiveTypeMap.getOrElse(sym, storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ct.int.Symbol]))
      }

      /**
       * When compiling Array.scala, the type parameter T is not erased and shows up in method
       * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
       */
      def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
        assert(sym.isType && isCompilingArray, sym)
        ObjectReference
      }

      tp.widenDealias match {
        case JavaArrayType(el) =>ArrayBType(el.toTypeKind(ct)(storage)) // Array type such as Array[Int] (kept by erasure)
        case t: TypeRef =>
          t.info match {

            case _ =>
              if(!t.symbol.isClass) nonClassTypeRefToBType(t.symbol)  // See comment on nonClassTypeRefToBType
              else primitiveOrClassToBType(t.symbol) // Common reference to a type such as scala.Int or java.lang.String
          }
        case Types.ClassInfo(_, sym, _, _, _)           => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes toTypeKind(moduleClassSymbol.info)

        case t: MethodType => // triggers for LabelDefs
          t.resultType.toTypeKind(ct)(storage)

        /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
         * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
         * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
         */
        case a @ AnnotatedType(_, t) =>
          debuglog(s"typeKind of annotated type $a")
          t.toTypeKind(ct)(storage)

        /* ExistentialType should (probably) be eliminated by erasure. We know they get here for
         * classOf constants:
         *   class C[T]
         *   class T { final val k = classOf[C[_]] }
         */
       /* case e @ ExistentialType(_, t) =>
          debuglog(s"typeKind of existential type $e")
          t.toTypeKind(ctx)(storage)*/

        /* The cases below should probably never occur. They are kept for now to avoid introducing
         * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
         * test suite don't produce any warning.
         */

        case tp =>
          ctx.warning(
            s"an unexpected type representation reached the compiler backend while compiling $currentUnit: $tp. " +
              "If possible, please file a bug on issues.scala-lang.org.")

          tp match {
            case ThisType(ArrayClass)               => ObjectReference // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
            case ThisType(sym)                      => storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ct.int.Symbol])
           // case t: SingletonType                 => primitiveOrClassToBType(t.classSymbol)
            case t: SingletonType                  => t.underlying.toTypeKind(ct)(storage)
            case t: RefinedType                   =>  t.parent.toTypeKind(ct)(storage) //parents.map(_.toTypeKind(ct)(storage).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b))
          }
      }
    }

    def summaryString: String = tp.showSummary

    def params: List[Symbol] = Nil // used only for emmiting annotations

    def parents: List[Type] = tp.parents
  }



  object Assign extends AssignDeconstructor {
    def _1: Tree = field.lhs
    def _2: Tree = field.rhs
  }

  object Select extends SelectDeconstructor {
    def _1: Tree = field.qualifier
    def _2: Name = field.name
  }

  object Apply extends ApplyDeconstructor {
    def _1: Tree = field.fun
    def _2: List[Tree] = field.args
  }

  object If extends IfDeconstructor {
    def _1: Tree = field.cond
    def _2: Tree = field.thenp
    def _3: Tree = field.elsep
  }

  object ValDef extends ValDefDeconstructor {
    def _1: Modifiers = field.mods
    def _2: Name = field.name
    def _3: Tree = field.tpt
    def _4: Tree = field.rhs
  }

  object ApplyDynamic extends ApplyDynamicDeconstructor {
    def _1: Tree = ???
    def _2: List[Tree] = ???
  }

  // todo: this product1s should also eventually become name-based pattn matching
  object Literal extends LiteralDeconstructor {
    def get = field.const
  }

  object Throw extends ThrowDeconstructor {
    def get = field.expr
  }

  object New extends NewDeconstructor {
    def get = field.tpt.tpe
  }

  object This extends ThisDeconstructor {
    def get = field.qual
    def apply(s: Symbol): This = tpd.This(s.asClass)
  }

  object Return extends ReturnDeconstructor {
    def get = field.expr
  }

  object Ident extends IdentDeconstructor {
    def get = field.name
  }

  object Alternative extends AlternativeDeconstructor {
    def get = field.trees
  }

  object Constant extends ConstantDeconstructor {
    def get = field.value
  }
  object ThrownException extends ThrownException {
    def unapply(a: Annotation): Option[Symbol] = None // todo
  }

  object Try extends TryDeconstructor {
    def _1: Tree = field.expr
    def _2: List[Tree] = field.cases
    def _3: Tree = field.finalizer
  }

  object LabelDef extends LabelDeconstructor {
    def _1: Name = field.name
    def _2: List[Symbol] = field.vparamss.flatMap(_.map(_.symbol))
    def _3: Tree = field.rhs

    override def unapply(s: LabelDef): DottyBackendInterface.this.LabelDef.type = {
      if(s.symbol is Flags.Label) this.field = s
      else this.field = null
      this
    }
  }

  object Typed extends TypedDeconstrutor {
    def _1: Tree = field.expr
    def _2: Tree = field.tpt
  }
  object Super extends SuperDeconstructor {
    def _1: Tree = field.qual
    def _2: Name = field.mix
  }
  object ArrayValue extends ArrayValueDeconstructor {
    def _1: Tree = ???
    def _2: List[Tree] = ???
  }
  object Match extends MatchDeconstructor {
    def _1: Tree = field.selector
    def _2: List[Tree] = field.cases
  }
  object Block extends BlockDeconstructor {
    def _1: List[Tree] = field.stats
    def _2: Tree = field.expr
  }
  object TypeApply extends TypeApplyDeconstructor {
    def _1: Tree = field.fun
    def _2: List[Tree] = field.args
  }
  object CaseDef extends CaseDeconstructor {
    def _1: Tree = field.pat
    def _2: Tree = field.guard
    def _3: Tree = field.body
  }

  object DefDef extends DefDefDeconstructor {
    def _1: Modifiers = field.mods
    def _2: Name = field.name
    def _3: List[TypeDef] = field.tparams
    def _4: List[List[ValDef]] = field.vparamss
    def _5: Tree = field.tpt
    def _6: Tree = field.rhs
  }

  object ModuleDef extends ModuleDefDeconstructor {
    def _1: Modifiers = ???
    def _2: Name = ???
    def _3: Tree = ???
  }

  object Template extends TemplateDeconstructor {
    def _1: List[Tree] = field.parents
    def _2: ValDef = field.self
    def _3: List[Tree] = field.constr :: field.body
  }

  object Bind extends BindDeconstructor {
    def _1: Name = field.name
    def _2: Tree = field.body
  }

  object ClassDef extends ClassDefDeconstructor {
    def _1: Modifiers = field.mods
    def _2: Name = field.name
    def _4: Template = field.rhs.asInstanceOf[Template]
    def _3: List[TypeDef] = Nil
  }

  object Closure extends ClosureDeconstructor {
    def _1 = field.env
    def _2 = field.meth
    def _3 = {
      val t = field.tpt.tpe.typeSymbol
      if(t.exists) t
      else {
        val arity = field.meth.tpe.widenDealias.paramTypes.size - _1.size
        ctx.requiredClass(("scala.compat.java8.JFunction"+arity).toTermName)
      }
    }
  }

  def currentUnit = ctx.compilationUnit
}