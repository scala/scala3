package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc
import dotty.tools.dotc.core.Flags.{termFlagSet}
import dotty.tools.dotc.transform.{Erasure, GenericSignatures}
import dotty.tools.dotc.transform.SymUtils._
import java.io.{File => _}

import scala.annotation.threadUnsafe
import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.reflect.ClassTag
import dotty.tools.dotc.util.WeakHashSet
import dotty.tools.io.AbstractFile
import scala.tools.asm.AnnotationVisitor
import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._
import Phases._

import dotty.tools.dotc.util
import dotty.tools.dotc.util.Spans
import Decorators._
import tpd._

import scala.tools.asm
import StdNames.{nme, str}
import NameKinds.{DefaultGetterName, ExpandedName}
import Names.TermName

class DottyBackendInterface(outputDirectory: AbstractFile, val superCallsMap: Map[Symbol, Set[ClassSymbol]])(implicit ctx: Context) {
  import Symbols.{toDenot, toClassDenot}
    // Dotty deviation: Need to (re-)import implicit decorators here because otherwise
    // they would be shadowed by the more deeply nested `symHelper` decorator.

  type Flags           = Long
  type ConstantTag = Int

  type Symbol          = Symbols.Symbol
  type Type            = Types.Type
  type Tree            = tpd.Tree
  type CompilationUnit = dotc.CompilationUnit
  type Constant        = Constants.Constant
  type Literal         = tpd.Literal
  type Position        = Spans.Span
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
  type Throw           = tpd.Apply
  type Labeled         = tpd.Labeled
  type Return          = tpd.Return
  type WhileDo         = tpd.WhileDo
  type Block           = tpd.Block
  type Typed           = tpd.Typed
  type Match           = tpd.Match
  type This            = tpd.This
  type CaseDef         = tpd.CaseDef
  type Alternative     = tpd.Alternative
  type DefDef          = tpd.DefDef
  type Template        = tpd.Template
  type Select          = tpd.Tree // Actually tpd.Select || tpd.Ident
  type Bind            = tpd.Bind
  type New             = tpd.New
  type Super           = tpd.Super
  type Annotation      = Annotations.Annotation
  type ArrayValue      = tpd.JavaSeqLiteral
  type Closure         = tpd.Closure

  val NoSymbol: Symbol = Symbols.NoSymbol
  val NoPosition: Position = Spans.NoSpan
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

   // require LambdaMetafactory: scalac uses getClassIfDefined, but we need those always.
  @threadUnsafe lazy val LambdaMetaFactory: ClassSymbol = ctx.requiredClass("java.lang.invoke.LambdaMetafactory")
  @threadUnsafe lazy val MethodHandle: ClassSymbol      = ctx.requiredClass("java.lang.invoke.MethodHandle")

  val nme_valueOf: Name = StdNames.nme.valueOf
  val nme_apply: TermName = StdNames.nme.apply
  val NothingClass: Symbol = defn.NothingClass
  val NullClass: Symbol = defn.NullClass
  val ObjectClass: Symbol = defn.ObjectClass
  val Object_Type: Type = defn.ObjectType
  val Throwable_Type: Type = defn.ThrowableType
  val Object_isInstanceOf: Symbol = defn.Any_isInstanceOf
  val Object_asInstanceOf: Symbol = defn.Any_asInstanceOf
  val Object_synchronized: Symbol = defn.Object_synchronized
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
  def isArrayClone(tree: Tree): Boolean = tree match {
    case SelectBI(qual, StdNames.nme.clone_) if qual.tpe.widen.isInstanceOf[JavaArrayType] => true
    case _ => false
  }

  val hashMethodSym: Symbol = NoSymbol // used to dispatch ## on primitives to ScalaRuntime.hash. Should be implemented by a miniphase
  val externalEqualsNumNum: Symbol = defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumNum)
  val externalEqualsNumChar: Symbol = NoSymbol // ctx.requiredMethod(BoxesRunTimeTypeRef, nme.equalsNumChar) // this method is private
  val externalEqualsNumObject: Symbol = defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumObject)
  val externalEquals: Symbol = defn.BoxesRunTimeModule.info.decl(nme.equals_).suchThat(toDenot(_).info.firstParamTypes.size == 2).symbol
  val MaxFunctionArity: Int = Definitions.MaxImplementedFunctionArity
  val FunctionClass: Array[Symbol] = defn.FunctionClassPerRun()
  val AbstractFunctionClass: Array[Symbol] = defn.AbstractFunctionClassPerRun()
  val PartialFunctionClass: Symbol = defn.PartialFunctionClass
  val AbstractPartialFunctionClass: Symbol = defn.AbstractPartialFunctionClass
  val String_valueOf: Symbol = defn.String_valueOf_Object
  @threadUnsafe lazy val Predef_classOf: Symbol = defn.ScalaPredefModule.requiredMethod(nme.classOf)

  @threadUnsafe lazy val AnnotationRetentionAttr: ClassSymbol = ctx.requiredClass("java.lang.annotation.Retention")
  @threadUnsafe lazy val AnnotationRetentionSourceAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("SOURCE")
  @threadUnsafe lazy val AnnotationRetentionClassAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("CLASS")
  @threadUnsafe lazy val AnnotationRetentionRuntimeAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("RUNTIME")
  @threadUnsafe lazy val JavaAnnotationClass: ClassSymbol = ctx.requiredClass("java.lang.annotation.Annotation")

  def boxMethods: Map[Symbol, Symbol] = defn.ScalaValueClasses().map{x => // @darkdimius Are you sure this should be a def?
    (x, Erasure.Boxing.boxMethod(x.asClass))
  }.toMap
  def unboxMethods: Map[Symbol, Symbol] =
    defn.ScalaValueClasses().map(x => (x, Erasure.Boxing.unboxMethod(x.asClass))).toMap

  def isSyntheticArrayConstructor(s: Symbol): Boolean = {
    s eq defn.newArrayMethod
  }

  def isBox(sym: Symbol): Boolean =
    Erasure.Boxing.isBox(sym) && sym.denot.owner != defn.UnitModuleClass
  def isUnbox(sym: Symbol): Boolean =
    Erasure.Boxing.isUnbox(sym) && sym.denot.owner != defn.UnitModuleClass

  val primitives: Primitives = new Primitives {
    val primitives = new DottyPrimitives(ctx)
    def getPrimitive(app: Apply, receiver: Type): Int = primitives.getPrimitive(app, receiver)

    def getPrimitive(sym: Symbol): Int = primitives.getPrimitive(sym)

    def isPrimitive(fun: Tree): Boolean = primitives.isPrimitive(fun)
  }

  def isRuntimeVisible(annot: Annotation): Boolean =
    if (toDenot(annotHelper(annot).atp.typeSymbol).hasAnnotation(AnnotationRetentionAttr))
      retentionPolicyOf(annot) == AnnotationRetentionRuntimeAttr
    else {
      // SI-8926: if the annotation class symbol doesn't have a @RetentionPolicy annotation, the
      // annotation is emitted with visibility `RUNTIME`
      // dotty bug: #389
      true
    }

  def shouldEmitAnnotation(annot: Annotation): Boolean = {
    annot.symbol.isJavaDefined &&
      retentionPolicyOf(annot) != AnnotationRetentionSourceAttr &&
      annot.args.isEmpty
  }

  private def retentionPolicyOf(annot: Annotation): Symbol =
    annot.atp.typeSymbol.getAnnotation(AnnotationRetentionAttr).
      flatMap(_.argumentConstant(0).map(_.symbolValue)).getOrElse(AnnotationRetentionClassAttr)

  private def normalizeArgument(arg: Tree): Tree = arg match {
    case Trees.NamedArg(_, arg1) => normalizeArgument(arg1)
    case Trees.Typed(arg1, _) => normalizeArgument(arg1)
    case _ => arg
  }

  private def emitArgument(av:   AnnotationVisitor,
                           name: String,
                           arg:  Tree, bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    val narg = normalizeArgument(arg)
    // Transformation phases are not run on annotation trees, so we need to run
    // `constToLiteral` at this point.
    val t = constToLiteral(narg)(ctx.withPhase(ctx.erasurePhase))
    t match {
      case Literal(const @ Constant(_)) =>
        const.tag match {
          case BooleanTag | ByteTag | ShortTag | CharTag | IntTag | LongTag | FloatTag | DoubleTag => av.visit(name, const.value)
          case StringTag =>
            assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
            av.visit(name, const.stringValue) // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
          case ClazzTag => av.visit(name, const.typeValue.toTypeKind(bcodeStore)(innerClasesStore).toASMType)
          case EnumTag =>
            val edesc = innerClasesStore.typeDescriptor(const.tpe.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the enumeration class.
            val evalue = const.symbolValue.name.mangledString // value the actual enumeration value.
            av.visitEnum(name, edesc, evalue)
        }
      case t: TypeApply if (t.fun.symbol == Predef_classOf) =>
        av.visit(name, t.args.head.tpe.classSymbol.denot.info.toTypeKind(bcodeStore)(innerClasesStore).toASMType)
      case Ident(nme.WILDCARD) =>
        // An underscore argument indicates that we want to use the default value for this parameter, so do not emit anything
      case t: tpd.RefTree if t.symbol.denot.owner.isAllOf(Flags.JavaEnumTrait) =>
        val edesc = innerClasesStore.typeDescriptor(t.tpe.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the enumeration class.
        val evalue = t.symbol.name.mangledString // value the actual enumeration value.
        av.visitEnum(name, edesc, evalue)
      case t: SeqLiteral =>
        val arrAnnotV: AnnotationVisitor = av.visitArray(name)
        for (arg <- t.elems) { emitArgument(arrAnnotV, null, arg, bcodeStore)(innerClasesStore) }
        arrAnnotV.visitEnd()

      case Apply(fun, args) if fun.symbol == defn.ArrayClass.primaryConstructor ||
        toDenot(fun.symbol).owner == defn.ArrayClass.linkedClass && fun.symbol.name == nme_apply =>
        val arrAnnotV: AnnotationVisitor = av.visitArray(name)

        var actualArgs = if (fun.tpe.isImplicitMethod) {
          // generic array method, need to get implicit argument out of the way
          fun.asInstanceOf[Apply].args
        } else args

        val flatArgs = actualArgs.flatMap { arg =>
          normalizeArgument(arg) match {
            case t: tpd.SeqLiteral => t.elems
            case e => List(e)
          }
        }
        for(arg <- flatArgs) {
          emitArgument(arrAnnotV, null, arg, bcodeStore)(innerClasesStore)
        }
        arrAnnotV.visitEnd()
/*
      case sb @ ScalaSigBytes(bytes) =>
        // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
        // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
        if (sb.fitsInOneString) {
          av.visit(name, BCodeAsmCommon.strEncode(sb))
        } else {
          val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
          for(arg <- BCodeAsmCommon.arrEncode(sb)) { arrAnnotV.visit(name, arg) }
          arrAnnotV.visitEnd()
        }          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.
*/
      case t @ Apply(constr, args) if t.tpe.derivesFrom(JavaAnnotationClass) =>
        val typ = t.tpe.classSymbol.denot.info
        val assocs = assocsFromApply(t)
        val desc = innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the nested annotation class
        val nestedVisitor = av.visitAnnotation(name, desc)
        emitAssocs(nestedVisitor, assocs, bcodeStore)(innerClasesStore)

      case t =>
        ctx.error(ex"Annotation argument is not a constant", t.sourcePos)
    }
  }

  def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)
                              (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val typ = annot.atp
      val assocs = annot.assocs
      val av = cw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  private def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, Object)], bcodeStore: BCodeHelpers)
                        (innerClasesStore: bcodeStore.BCInnerClassGen) = {
    for ((name, value) <- assocs)
      emitArgument(av, name.mangledString, value.asInstanceOf[Tree], bcodeStore)(innerClasesStore)
    av.visitEnd()
  }

  def emitAnnotations(mw: asm.MethodVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)
                              (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val typ = annot.atp
      val assocs = annot.assocs
      val av = mw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)
                              (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val typ = annot.atp
      val assocs = annot.assocs
      val av = fw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[Annotation]], bcodeStore: BCodeHelpers)
                                   (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    val annotationss = pannotss map (_ filter shouldEmitAnnotation)
    if (annotationss forall (_.isEmpty)) return
    for ((annots, idx) <- annotationss.zipWithIndex;
         annot <- annots) {
      val typ = annot.atp
      val assocs = annot.assocs
      val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(pannVisitor, assocs, bcodeStore)(innerClasesStore)
    }
  }

  def getAnnotPickle(jclassName: String, sym: Symbol): Option[Annotation] = None


  def getRequiredClass(fullname: String): Symbol = ctx.requiredClass(fullname)

  def getClassIfDefined(fullname: String): Symbol = NoSymbol // used only for android. todo: implement

  private def erasureString(clazz: Class[_]): String = {
    if (clazz.isArray) "Array[" + erasureString(clazz.getComponentType) + "]"
    else clazz.getName
  }

  def requiredClass[T](implicit evidence: ClassTag[T]): Symbol =
    ctx.requiredClass(erasureString(evidence.runtimeClass))

  def requiredModule[T](implicit evidence: ClassTag[T]): Symbol = {
    val moduleName = erasureString(evidence.runtimeClass)
    val className = if (moduleName.endsWith("$")) moduleName.dropRight(1)  else moduleName
    ctx.requiredModule(className)
  }

  def debuglog(msg: => String): Unit = ctx.debuglog(msg)
  def informProgress(msg: String): Unit = ctx.informProgress(msg)
  def log(msg: => String): Unit = ctx.log(msg)
  def error(pos: Position, msg: String): Unit = ctx.error(msg, sourcePos(pos))
  def warning(pos: Position, msg: String): Unit = ctx.warning(msg, sourcePos(pos))
  def abort(msg: String): Nothing = {
    ctx.error(msg)
    throw new RuntimeException(msg)
  }
  def sourcePos(pos: Position)(implicit ctx: Context): util.SourcePosition =
    ctx.source.atSpan(pos)

  def emitAsmp: Option[String] = None

  def dumpClasses: Option[String] =
    if (ctx.settings.Ydumpclasses.isDefault) None
    else Some(ctx.settings.Ydumpclasses.value)

  def noForwarders: Boolean = ctx.settings.XnoForwarders.value
  def debuglevel: Int = 3 // 0 -> no debug info; 1-> filename; 2-> lines; 3-> varnames
  def settings_debug: Boolean = ctx.settings.Ydebug.value
  def targetPlatform: String = ctx.settings.target.value

  val perRunCaches: Caches = new Caches {
    def newAnyRefMap[K <: AnyRef, V](): mutable.AnyRefMap[K, V] = new mutable.AnyRefMap[K, V]()
    def newWeakMap[K, V](): mutable.WeakHashMap[K, V] = new mutable.WeakHashMap[K, V]()
    def recordCache[T <: Clearable](cache: T): T = cache
    def newWeakSet[K >: Null <: AnyRef](): WeakHashSet[K] = new WeakHashSet[K]()
    def newMap[K, V](): mutable.HashMap[K, V] = new mutable.HashMap[K, V]()
    def newSet[K](): mutable.Set[K] = new mutable.HashSet[K]
  }

  val MODULE_INSTANCE_FIELD: String = str.MODULE_INSTANCE_FIELD

  def dropModule(str: String): String =
    if (!str.isEmpty && str.last == '$') str.take(str.length - 1) else str

  def newTermName(prefix: String): Name = prefix.toTermName

  val Flag_SYNTHETIC: Flags = Flags.Synthetic.bits
  val Flag_METHOD: Flags = Flags.Method.bits
  val ExcludedForwarderFlags: Flags = DottyBackendInterface.ExcludedForwarderFlags.bits

  def isQualifierSafeToElide(qual: Tree): Boolean = tpd.isIdempotentExpr(qual)

  private val desugared = new java.util.IdentityHashMap[Type, tpd.Select]

  def desugarIdent(i: Ident): Option[tpd.Select] = {
    var found = desugared.get(i.tpe)
    if (found == null) {
      tpd.desugarIdent(i) match {
        case sel: tpd.Select =>
          desugared.put(i.tpe, sel)
          found = sel
        case _ =>
      }
    }
    if (found == null) None else Some(found)
  }

  // todo: remove
  def isMaybeBoxed(sym: Symbol): Boolean = {
    (sym == ObjectClass) ||
      (sym == JavaSerializableClass) ||
      (sym == defn.ComparableClass) ||
      (sym derivesFrom BoxedNumberClass) ||
      (sym derivesFrom BoxedCharacterClass) ||
      (sym derivesFrom BoxedBooleanClass)
  }

  def getSingleOutput: Option[AbstractFile] = None // todo: implement

  // @M don't generate java generics sigs for (members of) implementation
  // classes, as they are monomorphic (TODO: ok?)
  private final def needsGenericSignature(sym: Symbol): Boolean = !(
    // pp: this condition used to include sym.hasexpandedname, but this leads
    // to the total loss of generic information if a private member is
    // accessed from a closure: both the field and the accessor were generated
    // without it.  This is particularly bad because the availability of
    // generic information could disappear as a consequence of a seemingly
    // unrelated change.
       ctx.base.settings.YnoGenericSig.value
    || sym.is(Flags.Artifact)
    || sym.isAllOf(Flags.LiftedMethod)
    || sym.is(Flags.Bridge)
  )

  private def verifySignature(sym: Symbol, sig: String)(implicit ctx: Context): Unit = {
    import scala.tools.asm.util.CheckClassAdapter
    def wrap(body: => Unit): Unit = {
      try body
      catch {
        case ex: Throwable =>
          ctx.error(i"""|compiler bug: created invalid generic signature for $sym in ${sym.denot.owner.showFullName}
                      |signature: $sig
                      |if this is reproducible, please report bug at https://github.com/lampepfl/dotty/issues
                   """.trim, sym.sourcePos)
          throw  ex
      }
    }

    wrap {
      if (sym.is(Flags.Method)) {
        CheckClassAdapter.checkMethodSignature(sig)
      }
      else if (sym.isTerm) {
        CheckClassAdapter.checkFieldSignature(sig)
      }
      else {
        CheckClassAdapter.checkClassSignature(sig)
      }
    }
  }

  /**
   * Generates the generic signature for `sym` before erasure.
   *
   * @param sym   The symbol for which to generate a signature.
   * @param owner The owner of `sym`.
   * @return The generic signature of `sym` before erasure, as specified in the Java Virtual
   *         Machine Specification, §4.3.4, or `null` if `sym` doesn't need a generic signature.
   * @see https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3.4
   */
  def getGenericSignature(sym: Symbol, owner: Symbol): String = {
    ctx.atPhase(ctx.erasurePhase) {
      val memberTpe =
        if (sym.is(Flags.Method)) sym.denot.info
        else owner.denot.thisType.memberInfo(sym)
      getGenericSignature(sym, owner, memberTpe).orNull
    }
  }

  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String = {
    // scala/bug#3452 Static forwarder generation uses the same erased signature as the method if forwards to.
    // By rights, it should use the signature as-seen-from the module class, and add suitable
    // primitive and value-class boxing/unboxing.
    // But for now, just like we did in mixin, we just avoid writing a wrong generic signature
    // (one that doesn't erase to the actual signature). See run/t3452b for a test case.

    val memberTpe = ctx.atPhase(ctx.erasurePhase) { moduleClass.denot.thisType.memberInfo(sym) }
    val erasedMemberType = TypeErasure.erasure(memberTpe)
    if (erasedMemberType =:= sym.denot.info)
      getGenericSignature(sym, moduleClass, memberTpe).orNull
    else null
  }

  private def getGenericSignature(sym: Symbol, owner: Symbol, memberTpe: Type)(implicit ctx: Context): Option[String] =
    if (needsGenericSignature(sym)) {
      val erasedTypeSym = TypeErasure.fullErasure(sym.denot.info).typeSymbol
      if (erasedTypeSym.isPrimitiveValueClass) {
        // Suppress signatures for symbols whose types erase in the end to primitive
        // value types. This is needed to fix #7416.
        None
      } else {
        val jsOpt = GenericSignatures.javaSig(sym, memberTpe)
        if (ctx.settings.XverifySignatures.value) {
          jsOpt.foreach(verifySignature(sym, _))
        }

        jsOpt
      }
    } else {
      None
    }



  def sourceFileFor(cu: CompilationUnit): String = cu.source.file.name



  implicit def positionHelper(a: Position): PositionHelper = new PositionHelper {
    def isDefined: Boolean = a.exists
    def line: Int = sourcePos(a).line + 1
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

    def pos: Position = a.span

    def isEmpty: Boolean = a.isEmpty

    def tpe: Type = a.tpe

    def exists(pred: (Tree) => Boolean): Boolean = a.find(pred).isDefined
  }


  implicit def annotHelper(a: Annotation): AnnotationHelper = new AnnotationHelper {
    def atp: Type = a.tree.tpe

    def assocs: List[(Name, Tree)] = assocsFromApply(a.tree)

    def symbol: Symbol = a.tree.symbol

    def args: List[Tree] = List.empty // those arguments to scala-defined annotations. they are never emitted
  }

  def assocsFromApply(tree: Tree): List[(Name, Tree)] = {
    tree match {
      case Block(_, expr) => assocsFromApply(expr)
      case Apply(fun, args) =>
        fun.tpe.widen match {
          case MethodType(names) =>
            (names zip args).filter {
              case (_, t: tpd.Ident) if (t.tpe.normalizedPrefix eq NoPrefix) => false
              case _ => true
            }
        }
    }
  }

  implicit def nameHelper(n: Name): NameHelper = new NameHelper {
    def isTypeName: Boolean = n.isTypeName
    def isTermName: Boolean = n.isTermName
    def startsWith(s: String): Boolean = n.startsWith(s)
    def mangledString: String = n.mangledString
  }

  implicit def symHelper(sym: Symbol): SymbolHelper = new SymbolHelper {
    def exists: Boolean = sym.exists

    // names
    def showFullName: String = sym.showFullName
    def javaSimpleName: String = toDenot(sym).name.mangledString // addModuleSuffix(simpleName.dropLocal)
    def javaBinaryName: String = javaClassName.replace('.', '/') // TODO: can we make this a string? addModuleSuffix(fullNameInternal('/'))
    def javaClassName: String = toDenot(sym).fullName.mangledString // addModuleSuffix(fullNameInternal('.')).toString
    def name: Name = sym.name
    def rawname: String = {
      val original = toDenot(sym).initial
      sym.name(ctx.withPhase(original.validFor.phaseId)).mangledString
    }

    // types
    def info: Type = toDenot(sym).info
    def tpe: Type = toDenot(sym).info // todo whats the differentce between tpe and info?
    def thisType: Type = toDenot(sym).thisType

    // tests
    def isClass: Boolean = {
      sym.isPackageObject || (sym.isClass)
    }
    def isType: Boolean = sym.isType
    def isAnonymousClass: Boolean = toDenot(sym).isAnonymousClass
    def isConstructor: Boolean = toDenot(sym).isConstructor
    def isExpanded: Boolean = sym.name.is(ExpandedName)
    def isAnonymousFunction: Boolean = toDenot(sym).isAnonymousFunction
    def isMethod: Boolean = sym.is(Flags.Method)
    def isPublic: Boolean = !sym.flags.isOneOf(Flags.Private | Flags.Protected)
    def isSynthetic: Boolean = sym.is(Flags.Synthetic)
    def isPackageClass: Boolean = sym.is(Flags.PackageClass)
    def isModuleClass: Boolean = sym.is(Flags.ModuleClass)
    def isModule: Boolean = sym.is(Flags.Module)
    def isStrictFP: Boolean = false // todo: implement
    def isLabel: Boolean = sym.is(Flags.Label)
    def hasPackageFlag: Boolean = sym.is(Flags.Package)
    def isInterface: Boolean = (sym.is(Flags.PureInterface)) || (sym.is(Flags.Trait))
    def isGetter: Boolean = toDenot(sym).isGetter
    def isSetter: Boolean = toDenot(sym).isSetter
    def isGetClass: Boolean = sym eq defn.Any_getClass
    def isJavaDefined: Boolean = sym.is(Flags.JavaDefined)
    def isJavaDefaultMethod: Boolean = !((sym.is(Flags.Deferred))  || toDenot(sym).isClassConstructor)
    def isDeferred: Boolean = sym.is(Flags.Deferred)
    def isPrivate: Boolean = sym.is(Flags.Private)
    def getsJavaFinalFlag: Boolean =
      isFinal &&  !toDenot(sym).isClassConstructor && !(sym.is(Flags.Mutable)) &&  !(sym.enclosingClass.is(Flags.Trait))

    def getsJavaPrivateFlag: Boolean =
      isPrivate || (sym.isPrimaryConstructor && sym.owner.isTopLevelModuleClass)

    def isFinal: Boolean = sym.is(Flags.Final)
    def isScalaStatic: Boolean =
      toDenot(sym).hasAnnotation(ctx.definitions.ScalaStaticAnnot)
    def isStaticMember: Boolean = (sym ne NoSymbol) &&
      (sym.is(Flags.JavaStatic) || isScalaStatic)
      // guard against no sumbol cause this code is executed to select which call type(static\dynamic) to use to call array.clone

    def isBottomClass: Boolean = (sym eq defn.NullClass) || (sym eq defn.NothingClass)
    def isBridge: Boolean = sym.is(Flags.Bridge)
    def isArtifact: Boolean = sym.is(Flags.Artifact)
    def hasEnumFlag: Boolean = sym.isAllOf(Flags.JavaEnumTrait)
    def hasAccessBoundary: Boolean = sym.accessBoundary(defn.RootClass) ne defn.RootClass
    def isVarargsMethod: Boolean = sym.is(Flags.JavaVarargs)
    def isDeprecated: Boolean = false
    def isMutable: Boolean = sym.is(Flags.Mutable)
    def hasAbstractFlag: Boolean = sym.isOneOf(Flags.AbstractOrTrait)
    def hasModuleFlag: Boolean = sym.is(Flags.Module)
    def isSynchronized: Boolean = sym.is(Flags.Synchronized)
    def isNonBottomSubClass(other: Symbol): Boolean = sym.derivesFrom(other)
    def hasAnnotation(ann: Symbol): Boolean = toDenot(sym).hasAnnotation(ann)
    def shouldEmitForwarders: Boolean =
      (sym.is(Flags.Module)) && sym.isStatic
    def isEnum = sym.is(Flags.Enum)

    def isClassConstructor: Boolean = toDenot(sym).isClassConstructor
    def isAnnotation: Boolean = toDenot(sym).isAnnotation
    def isSerializable: Boolean = toDenot(sym).isSerializable

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean =
      (sym.is(Flags.Module)) && {
        // scalac uses atPickling here
        // this would not work if modules are created after pickling
        // for example by specialization
        val original = toDenot(sym).initial
        val validity = original.validFor
        val shiftedContext = ctx.withPhase(validity.phaseId)
        toDenot(sym)(shiftedContext).isStatic(shiftedContext)
      }

    def isStaticConstructor: Boolean = (isStaticMember && isClassConstructor) || (sym.name eq nme.STATIC_CONSTRUCTOR)


    // navigation
    def owner: Symbol = toDenot(sym).owner
    def rawowner: Symbol = {
      originalOwner.originalLexicallyEnclosingClass
    }
    def originalOwner: Symbol = toDenot(sym).originalOwner
    def parentSymbols: List[Symbol] = toDenot(sym).info.parents.map(_.typeSymbol)
    def superClass: Symbol =  {
      val t = toDenot(sym).asClass.superClass
      if (t.exists) t
      else if (sym.is(Flags.ModuleClass)) {
        // workaround #371

        println(s"Warning: mocking up superclass for $sym")
        ObjectClass
      }
      else t
    }
    def enclClass: Symbol = toDenot(sym).enclosingClass
    def linkedClassOfClass: Symbol = linkedClass
    def linkedClass: Symbol = toDenot(sym)(ctx).linkedClass(ctx) //exitingPickler(sym.linkedClassOfClass)
    def companionClass: Symbol = toDenot(sym).companionClass
    def companionModule: Symbol = toDenot(sym).companionModule
    def companionSymbol: Symbol = if (sym.is(Flags.Module)) companionClass else companionModule
    def moduleClass: Symbol = toDenot(sym).moduleClass
    def enclosingClassSym: Symbol = {
      if (this.isClass) {
        val ct = ctx.withPhase(ctx.flattenPhase.prev)
        toDenot(sym)(ct).owner.enclosingClass(ct)
      }
      else sym.enclosingClass(ctx.withPhase(ctx.flattenPhase.prev))
    } //todo is handled specially for JavaDefined symbols in scalac
    def originalLexicallyEnclosingClass: Symbol =
      // used to populate the EnclosingMethod attribute.
      // it is very tricky in presence of classes(and annonymous classes) defined inside supper calls.
      if (sym.exists) {
        val validity = toDenot(sym).initial.validFor
        val shiftedContext = ctx.withPhase(validity.phaseId)
        toDenot(sym)(shiftedContext).lexicallyEnclosingClass(shiftedContext)
      } else NoSymbol
    def nextOverriddenSymbol: Symbol = toDenot(sym).nextOverriddenSymbol
    def allOverriddenSymbols: List[Symbol] = toDenot(sym).allOverriddenSymbols.toList

    // members
    def primaryConstructor: Symbol = toDenot(sym).primaryConstructor

    /** For currently compiled classes: All locally defined classes including local classes.
     *  The empty list for classes that are not currently compiled.

     */
    def nestedClasses: List[Symbol] = definedClasses(ctx.flattenPhase)

    /** For currently compiled classes: All classes that are declared as members of this class
     *  (but not inherited ones). The empty list for classes that are not currently compiled.
     */
    def memberClasses: List[Symbol] = definedClasses(ctx.lambdaLiftPhase)

    private def definedClasses(phase: Phase) =
      if (sym.isDefinedInCurrentRun)
        ctx.atPhase(phase) {
          toDenot(sym).info.decls.filter(_.isClass)
        }
      else Nil

    def annotations: List[Annotation] = toDenot(sym).annotations
    def companionModuleMembers: List[Symbol] =  {
      // phase travel to exitingPickler: this makes sure that memberClassesOf only sees member classes,
      // not local classes of the companion module (E in the example) that were lifted by lambdalift.
      if (linkedClass.isTopLevelModuleClass) /*exitingPickler*/ linkedClass.memberClasses
      else Nil
    }
    def fieldSymbols: List[Symbol] = {
      toDenot(sym).info.decls.filter(p => p.isTerm && !p.is(Flags.Method))
    }
    def methodSymbols: List[Symbol] =
      for (f <- toDenot(sym).info.decls.toList if f.isMethod && f.isTerm && !f.isModule) yield f
    def serialVUID: Option[Long] =
      sym.getAnnotation(defn.SerialVersionUIDAnnot).flatMap { annot =>
        if (sym.is(Flags.Trait)) {
          ctx.error("@SerialVersionUID does nothing on a trait", annot.tree.sourcePos)
          None
        } else {
          val vuid = annot.argumentConstant(0).map(_.longValue)
          if (vuid.isEmpty)
            ctx.error("The argument passed to @SerialVersionUID must be a constant",
              annot.argument(0).getOrElse(annot.tree).sourcePos)
          vuid
        }
      }

    def freshLocal(cunit: CompilationUnit, name: String, tpe: Type, pos: Position, flags: Flags): Symbol = {
      ctx.newSymbol(sym, name.toTermName, termFlagSet(flags), tpe, NoSymbol, pos)
    }

    def getter(clz: Symbol): Symbol = decorateSymbol(sym).getter
    def setter(clz: Symbol): Symbol = decorateSymbol(sym).setter

    def moduleSuffix: String = "" // todo: validate that names already have $ suffix
    def outputDirectory: AbstractFile = DottyBackendInterface.this.outputDirectory
    def pos: Position = sym.span

    def throwsAnnotations: List[Symbol] = Nil

    /**
     * All interfaces implemented by a class, except for those inherited through the superclass.
     * Redundant interfaces are removed unless there is a super call to them.
     */
    def superInterfaces: List[Symbol] = {
      val directlyInheritedTraits = decorateSymbol(sym).directlyInheritedTraits
      val directlyInheritedTraitsSet = directlyInheritedTraits.toSet
      val allBaseClasses = directlyInheritedTraits.iterator.flatMap(_.asClass.baseClasses.drop(1)).toSet
      val superCalls = superCallsMap.getOrElse(sym, Set.empty)
      val additional = (superCalls -- directlyInheritedTraitsSet).filter(_.is(Flags.Trait))
//      if (additional.nonEmpty)
//        println(s"$fullName: adding supertraits $additional")
      directlyInheritedTraits.filter(t => !allBaseClasses(t) || superCalls(t)) ++ additional
    }

    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean = sym.isModuleClass &&
      ctx.atPhase(ctx.flattenPhase) {
        toDenot(sym).owner.is(Flags.PackageClass)
      }

    def addRemoteRemoteExceptionAnnotation: Unit = ()

    def samMethod(): Symbol = ctx.atPhase(ctx.erasurePhase) {
      val samMethods = toDenot(sym).info.possibleSamMethods.toList
      samMethods match {
        case x :: Nil => x.symbol
        case Nil => abort(s"${sym.show} is not a functional interface. It doesn't have abstract methods")
        case xs => abort(s"${sym.show} is not a functional interface. " +
          s"It has the following abstract methods: ${xs.map(_.name).mkString(", ")}")
      }
    }

    def isFunctionClass: Boolean =
      defn.isFunctionClass(sym)
  }


  implicit def typeHelper(tp: Type): TypeHelper = new TypeHelper {
    def member(string: Name): Symbol = tp.member(string.toTermName).symbol

    def isFinalType: Boolean = tp.typeSymbol.is(Flags.Final) //in scalac checks for type parameters. Why? Aren't they gone by backend?

    def underlying: Type = tp match {
      case t: TypeProxy => t.underlying
      case _ => tp
    }

    def paramTypes: List[Type] = tp.firstParamTypes

    def <:<(other: Type): Boolean = tp <:< other

    def memberInfo(s: Symbol): Type = tp.memberInfo(s)

    def decl(name: Name): Symbol = tp.decl(name).symbol

    def decls: List[Symbol] = tp.decls.toList

    def members: List[Symbol] = tp.allMembers.map(_.symbol).toList

    def typeSymbol: Symbol = tp.widenDealias.typeSymbol

    def =:=(other: Type): Boolean = tp =:= other

    def sortedMembersBasedOnFlags(required: Flags, excluded: Flags): List[Symbol] = {
      val requiredFlagSet = termFlagSet(required)
      val excludedFlagSet = termFlagSet(excluded)
      // The output of `memberNames` is a Set, sort it to guarantee a stable ordering.
      val names = tp.memberNames(takeAllFilter).toSeq.sorted
      val buffer = mutable.ListBuffer[Symbol]()
      names.foreach { name =>
        buffer ++= tp.memberBasedOnFlags(name, requiredFlagSet, excludedFlagSet)
          .alternatives.sortBy(_.signature)(Signature.lexicographicOrdering).map(_.symbol)
      }
      buffer.toList
    }

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
        primitiveTypeMap.getOrElse(sym.asInstanceOf[ct.bTypes.coreBTypes.bTypes.int.Symbol],
          storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ct.int.Symbol])).asInstanceOf[BType]
      }

      /**
       * When compiling Array.scala, the type parameter T is not erased and shows up in method
       * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
       */
      def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
        assert(sym.isType && isCompilingArray, sym)
        ObjectReference.asInstanceOf[ct.bTypes.ClassBType]
      }

      tp.widenDealias match {
        case JavaArrayType(el) =>ArrayBType(el.toTypeKind(ct)(storage)) // Array type such as Array[Int] (kept by erasure)
        case t: TypeRef =>
          t.info match {

            case _ =>
              if (!t.symbol.isClass) nonClassTypeRefToBType(t.symbol)  // See comment on nonClassTypeRefToBType
              else primitiveOrClassToBType(t.symbol) // Common reference to a type such as scala.Int or java.lang.String
          }
        case Types.ClassInfo(_, sym, _, _, _)           => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes toTypeKind(moduleClassSymbol.info)

        /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
         * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
         * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
         */
        case a @ AnnotatedType(t, _) =>
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
              "If possible, please file a bug on https://github.com/lampepfl/dotty/issues")

          tp match {
            case tp: ThisType if tp.cls == ArrayClass => ObjectReference.asInstanceOf[ct.bTypes.ClassBType] // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
            case tp: ThisType                         => storage.getClassBTypeAndRegisterInnerClass(tp.cls.asInstanceOf[ct.int.Symbol])
           // case t: SingletonType                   => primitiveOrClassToBType(t.classSymbol)
            case t: SingletonType                     => t.underlying.toTypeKind(ct)(storage)
            case t: RefinedType                       =>  t.parent.toTypeKind(ct)(storage) //parents.map(_.toTypeKind(ct)(storage).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b))
          }
      }
    }

    def summaryString: String = tp.showSummary()

    def params: List[Symbol] =
      Nil // backend uses this to emit annotations on parameter lists of forwarders
          // to static methods of companion class
          // in Dotty this link does not exists: there is no way to get from method type
          // to inner symbols of DefDef
          // todo: somehow handle.

    def parents: List[Type] = tp.parents
  }

  object SelectBI extends SelectDeconstructor {

    var desugared: tpd.Select = null

    override def isEmpty: Boolean =
      desugared eq null

    def _1: Tree =  desugared.qualifier

    def _2: Name = desugared.name

    override def unapply(s: Select): this.type = {
      s match {
        case t: tpd.Select => desugared = t
        case t: Ident  =>
          desugarIdent(t) match {
            case Some(t) => desugared = t
            case None => desugared = null
          }
        case _ => desugared = null
      }

      this
    }
  }

  object ValDefBI extends ValDefDeconstructor {
    def _1: Null = null
    def _2: Name = field.name
    def _3: Tree = field.tpt
    def _4: Tree = field.rhs
  }

  object ThrowBI extends ThrowDeconstructor {
    def get: Tree = field.args.head

    override def unapply(s: Throw): ThrowBI.type = {
      if (s.fun.symbol eq defn.throwMethod) {
        field = s
      } else {
        field = null
      }
      this
    }
  }

  object NewBI extends NewDeconstructor {
    def get: Type = field.tpt.tpe
  }

  object ThisBI extends ThisDeconstructor {
    def get: Name = field.qual.name
    def apply(s: Symbol): This = tpd.This(s.asClass)
  }

  object ReturnBI extends ReturnDeconstructor {
    def _1: Tree = field.expr
    def _2: Symbol = if (field.from.symbol.isLabel) field.from.symbol else NoSymbol
  }

  object ConstantBI extends ConstantDeconstructor {
    def get: Any = field.value
  }
  object ThrownExceptionBI extends ThrownException {
    def unapply(a: Annotation): Option[Symbol] = None // todo
  }

  object TryBI extends TryDeconstructor {
    def _1: Tree = field.expr
    def _2: List[Tree] = field.cases
    def _3: Tree = field.finalizer
  }

  object SuperBI extends SuperDeconstructor {
    def _1: Tree = field.qual
    def _2: Name = field.mix.name
  }
  object ArrayValueBI extends ArrayValueDeconstructor {
    def _1: Type = field.tpe match {
      case JavaArrayType(elem) => elem
      case _ =>
        error(field.span, s"JavaSeqArray with type ${field.tpe} reached backend: $field")
        UnspecifiedErrorType
    }
    def _2: List[Tree] = field.elems
  }

  object DefDefBI extends DefDefDeconstructor {
    def _1: Null = null
    def _2: Name = field.name
    def _3: List[TypeDef] = field.tparams
    def _4: List[List[ValDef]] = field.vparamss
    def _5: Tree = field.tpt
    def _6: Tree = field.rhs
  }

  object TemplateBI extends TemplateDeconstructor {
    def _1: List[Tree] = field.parents
    def _2: ValDef = field.self
    def _3: List[Tree] =
      if (field.constr.rhs.isEmpty) field.body
      else field.constr :: field.body
  }

  object ClassDefBI extends ClassDefDeconstructor {
    def _1: Null = null
    def _2: Name = field.name
    def _4: Template = field.rhs.asInstanceOf[Template]
    def _3: List[TypeDef] = Nil
  }

  object ClosureBI extends ClosureDeconstructor {
    def _1: List[Tree] = field.env
    def _2: Tree = field.meth
    def _3: Symbol = {
      val t = field.tpt.tpe.typeSymbol
      if (t.exists) t
      else {
        val arity = field.meth.tpe.widenDealias.paramTypes.size - _1.size
        val returnsUnit = field.meth.tpe.widenDealias.resultType.classSymbol == UnitClass
        if (returnsUnit) ctx.requiredClass(("dotty.runtime.function.JProcedure" + arity))
        else if (arity <= 2) ctx.requiredClass(("dotty.runtime.function.JFunction" + arity))
        else ctx.requiredClass(("scala.Function" + arity))
      }
    }
  }

  def currentUnit: CompilationUnit = ctx.compilationUnit



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
    def _1: Null
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

  abstract class DefDefDeconstructor extends DeconstructorCommon[DefDef]{
    def _1: Null
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
    def _1: Null
    def _2: Name
    def _3: Tree
    def _4: Tree
  }


  abstract class TryDeconstructor extends DeconstructorCommon[Try]{
    def _1: Tree
    def _2: List[Tree]
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
    def exists: Boolean

    // names
    def showFullName: String
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
     *  create for Java-defined classes as well as for Java annotations
     *  which we represent as classes.
     */
    final def isEmittedInterface: Boolean = isInterface ||
      isJavaDefined && (isAnnotation || isModuleClass && symHelper(companionClass).isInterface)

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
    def isScalaStatic: Boolean
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
    def isJavaDefaultMethod: Boolean
    def isClassConstructor: Boolean
    def isAnnotation: Boolean
    def isSerializable: Boolean
    def isEnum: Boolean

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
    def allOverriddenSymbols: List[Symbol]


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
      isPackageClass || isModuleClass && symHelper(symHelper(originalOwner).originalLexicallyEnclosingClass).isOriginallyStaticOwner

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

    /** The members of this type that have all of `required` flags but none of `excluded` flags set.
     *  The members are sorted by name and signature to guarantee a stable ordering.
     */
    def sortedMembersBasedOnFlags(required: Flags, excluded: Flags): List[Symbol]
    def members: List[Symbol]
    def decl(name: Name): Symbol
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
    def getPrimitive(app: Apply, receiver: Type): Int
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

  abstract class Caches {
    def recordCache[T <: Clearable](cache: T): T
    def newWeakMap[K, V](): collection.mutable.WeakHashMap[K, V]
    def newMap[K, V](): collection.mutable.HashMap[K, V]
    def newSet[K](): collection.mutable.Set[K]
    def newWeakSet[K >: Null <: AnyRef](): dotty.tools.dotc.util.WeakHashSet[K]
    def newAnyRefMap[K <: AnyRef, V](): collection.mutable.AnyRefMap[K, V]
  }

  // Class symbols used in backend.
  // Vals because they are to frequent in scala programs so that they are already loaded by backend

  lazy val NativeAttr: Symbol = requiredClass[scala.native]
  lazy val TransientAttr = requiredClass[scala.transient]
  lazy val VolatileAttr = requiredClass[scala.volatile]

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
  val StringModule: Symbol = symHelper(StringClass).linkedClassOfClass
  val ScalaRunTimeModule: Symbol = requiredModule[scala.runtime.ScalaRunTime.type]


  def isNull(t: Tree): Boolean = t match {
    case Literal(Constant(null)) => true
    case _ => false
  }
  def isLiteral(t: Tree): Boolean = t match {
    case Literal(_) => true
    case _ => false
  }
  def isNonNullExpr(t: Tree): Boolean = isLiteral(t) || ((treeHelper(t).symbol ne null) && symHelper(treeHelper(t).symbol).isModule)
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

object DottyBackendInterface {
  val ExcludedForwarderFlags: Flags.FlagSet = {
    Flags.Specialized | Flags.Lifted | Flags.Protected | Flags.JavaStatic |
    Flags.Private | Flags.Macro
  }
}
