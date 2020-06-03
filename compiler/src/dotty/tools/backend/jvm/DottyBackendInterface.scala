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
import Constants._
import tpd._

import scala.tools.asm
import StdNames.{nme, str}
import NameKinds.{DefaultGetterName, ExpandedName}
import Names.TermName

class DottyBackendInterface(val outputDirectory: AbstractFile, val superCallsMap: Map[Symbol, Set[ClassSymbol]])(implicit val ctx: Context) {
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

   // require LambdaMetafactory: scalac uses getClassIfDefined, but we need those always.
  @threadUnsafe lazy val LambdaMetaFactory: ClassSymbol = ctx.requiredClass("java.lang.invoke.LambdaMetafactory")
  @threadUnsafe lazy val MethodHandle: ClassSymbol      = ctx.requiredClass("java.lang.invoke.MethodHandle")

  def isArrayClone(tree: Tree): Boolean = tree match {
    case SelectBI(qual, StdNames.nme.clone_) if qual.tpe.widen.isInstanceOf[JavaArrayType] => true
    case _ => false
  }

  val externalEquals: Symbol = defn.BoxesRunTimeModule.info.decl(nme.equals_).suchThat(toDenot(_).info.firstParamTypes.size == 2).symbol

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

  val primitives = new DottyPrimitives(ctx)

  def isRuntimeVisible(annot: Annotation): Boolean =
    if (toDenot(annot.tree.tpe.typeSymbol).hasAnnotation(AnnotationRetentionAttr))
      retentionPolicyOf(annot) == AnnotationRetentionRuntimeAttr
    else {
      // SI-8926: if the annotation class symbol doesn't have a @RetentionPolicy annotation, the
      // annotation is emitted with visibility `RUNTIME`
      // dotty bug: #389
      true
    }

  def shouldEmitAnnotation(annot: Annotation): Boolean = {
    annot.symbol.is(Flags.JavaDefined) &&
      retentionPolicyOf(annot) != AnnotationRetentionSourceAttr
  }

  private def retentionPolicyOf(annot: Annotation): Symbol =
    annot.tree.tpe.typeSymbol.getAnnotation(AnnotationRetentionAttr).
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
          case ClazzTag => av.visit(name, typeToTypeKind(const.typeValue)(bcodeStore)(innerClasesStore).toASMType)
          case EnumTag =>
            val edesc = innerClasesStore.typeDescriptor(const.tpe.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the enumeration class.
            val evalue = const.symbolValue.name.mangledString // value the actual enumeration value.
            av.visitEnum(name, edesc, evalue)
        }
      case t: TypeApply if (t.fun.symbol == defn.Predef_classOf) =>
        av.visit(name, typeToTypeKind(t.args.head.tpe.classSymbol.denot.info)(bcodeStore)(innerClasesStore).toASMType)
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
        toDenot(fun.symbol).owner == defn.ArrayClass.linkedClass && fun.symbol.name == nme.apply =>
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
      val typ = annot.tree.tpe
      val assocs = assocsFromApply(annot.tree)
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
      val typ = annot.tree.tpe
      val assocs = assocsFromApply(annot.tree)
      val av = mw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)
                              (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val typ = annot.tree.tpe
      val assocs = assocsFromApply(annot.tree)
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
      val typ = annot.tree.tpe
      val assocs = assocsFromApply(annot.tree)
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

  def error(pos: Position, msg: String): Unit = ctx.error(msg, sourcePos(pos))
  def warning(pos: Position, msg: String): Unit = ctx.warning(msg, sourcePos(pos))
  def abort(msg: String): Nothing = {
    ctx.error(msg)
    throw new RuntimeException(msg)
  }
  def sourcePos(pos: Position)(implicit ctx: Context): util.SourcePosition =
    ctx.source.atSpan(pos)

  def dumpClasses: Option[String] =
    if (ctx.settings.Ydumpclasses.isDefault) None
    else Some(ctx.settings.Ydumpclasses.value)

  def debuglevel: Int = 3 // 0 -> no debug info; 1-> filename; 2-> lines; 3-> varnames

  val perRunCaches: Caches = new Caches {
    def newAnyRefMap[K <: AnyRef, V](): mutable.AnyRefMap[K, V] = new mutable.AnyRefMap[K, V]()
    def newWeakMap[K, V](): mutable.WeakHashMap[K, V] = new mutable.WeakHashMap[K, V]()
    def recordCache[T <: Clearable](cache: T): T = cache
    def newWeakSet[K >: Null <: AnyRef](): WeakHashSet[K] = new WeakHashSet[K]()
    def newMap[K, V](): mutable.HashMap[K, V] = new mutable.HashMap[K, V]()
    def newSet[K](): mutable.Set[K] = new mutable.HashSet[K]
  }

  def dropModule(str: String): String =
    if (!str.isEmpty && str.last == '$') str.take(str.length - 1) else str

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
    (sym == defn.ObjectClass) ||
      (sym == defn.JavaSerializableClass) ||
      (sym == defn.ComparableClass) ||
      (sym derivesFrom defn.BoxedNumberClass) ||
      (sym derivesFrom defn.BoxedCharClass) ||
      (sym derivesFrom defn.BoxedBooleanClass)
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

  implicit def symHelper(sym: Symbol): SymbolHelper = new SymbolHelper(sym)

  class SymbolHelper(sym: Symbol) {

    // tests
    def isClass: Boolean = {
      sym.isPackageObject || (sym.isClass)
    }
    def isPublic: Boolean = !sym.flags.isOneOf(Flags.Private | Flags.Protected)
    def isStrictFP: Boolean = false // todo: implement
    def hasPackageFlag: Boolean = sym.is(Flags.Package)
    def isInterface: Boolean = (sym.is(Flags.PureInterface)) || (sym.is(Flags.Trait))
    def isJavaDefaultMethod: Boolean = !((sym.is(Flags.Deferred))  || toDenot(sym).isClassConstructor)

    /** Does this symbol actually correspond to an interface that will be emitted?
     *  In the backend, this should be preferred over `isInterface` because it
     *  also returns true for the symbols of the fake companion objects we
     *  create for Java-defined classes as well as for Java annotations
     *  which we represent as classes.
     */
    def isEmittedInterface: Boolean = isInterface ||
      sym.is(Flags.JavaDefined) && (toDenot(sym).isAnnotation || sym.is(Flags.ModuleClass) && symHelper(sym.companionClass).isInterface)

    def isScalaStatic: Boolean =
      toDenot(sym).hasAnnotation(ctx.definitions.ScalaStaticAnnot)
    def isStaticMember: Boolean = (sym ne NoSymbol) &&
      (sym.is(Flags.JavaStatic) || isScalaStatic)
      // guard against no sumbol cause this code is executed to select which call type(static\dynamic) to use to call array.clone

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

    def isStaticConstructor: Boolean = (isStaticMember && sym.isClassConstructor) || (sym.name eq nme.STATIC_CONSTRUCTOR)


    // navigation
    def superClass: Symbol =  {
      val t = toDenot(sym).asClass.superClass
      if (t.exists) t
      else if (sym.is(Flags.ModuleClass)) {
        // workaround #371

        println(s"Warning: mocking up superclass for $sym")
        defn.ObjectClass
      }
      else t
    }
    // def enclClass: Symbol = toDenot(sym).enclosingClass
    def linkedClassOfClass: Symbol = sym.linkedClass
    // def linkedClass: Symbol = toDenot(sym)(ctx).linkedClass(ctx) //exitingPickler(sym.linkedClassOfClass)
    // def companionClass: Symbol = toDenot(sym).companionClass
    // def companionModule: Symbol = toDenot(sym).companionModule
    def companionSymbol: Symbol = if (sym.is(Flags.Module)) sym.companionClass else sym.companionModule
    // def moduleClass: Symbol = toDenot(sym).moduleClass
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

    // members

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

    def companionModuleMembers: List[Symbol] =  {
      // phase travel to exitingPickler: this makes sure that memberClassesOf only sees member classes,
      // not local classes of the companion module (E in the example) that were lifted by lambdalift.
      if (sym.linkedClass.isTopLevelModuleClass) /*exitingPickler*/ sym.linkedClass.memberClasses
      else Nil
    }
    def fieldSymbols: List[Symbol] = {
      toDenot(sym).info.decls.filter(p => p.isTerm && !p.is(Flags.Method))
    }
    def methodSymbols: List[Symbol] =
      for (f <- toDenot(sym).info.decls.toList if f.is(Flags.Method) && f.isTerm && !f.is(Flags.Module)) yield f


    // def freshLocal(cunit: CompilationUnit, name: String, tpe: Type, pos: Position, flags: Flags): Symbol = {
    //   ctx.newSymbol(sym, name.toTermName, termFlagSet(flags), tpe, NoSymbol, pos)
    // }

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
    def isTopLevelModuleClass: Boolean = sym.is(Flags.ModuleClass) &&
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
      sym.is(Flags.PackageClass) || sym.is(Flags.ModuleClass) && symHelper(symHelper(sym.originalOwner).originalLexicallyEnclosingClass).isOriginallyStaticOwner
  }


  /** The members of this type that have all of `required` flags but none of `excluded` flags set.
   *  The members are sorted by name and signature to guarantee a stable ordering.
   */
  def sortedMembersBasedOnFlags(tp: Type, required: Flags, excluded: Flags): List[Symbol] = {
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

  /**
   * This method returns the BType for a type reference, for example a parameter type.
   *
   * If the result is a ClassBType for a nested class, it is added to the innerClassBufferASM.
   *
   * If `t` references a class, toTypeKind ensures that the class is not an implementation class.
   * See also comment on getClassBTypeAndRegisterInnerClass, which is invoked for implementation
   * classes.
   */
  def typeToTypeKind(tp: Type)(ct: BCodeHelpers)(storage: ct.BCInnerClassGen): ct.bTypes.BType = {
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
      assert(sym != defn.ArrayClass || isCompilingArray, sym)
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
      case JavaArrayType(el) =>ArrayBType(typeToTypeKind(el)(ct)(storage)) // Array type such as Array[Int] (kept by erasure)
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
        ctx.debuglog(s"typeKind of annotated type $a")
        typeToTypeKind(t)(ct)(storage)

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
          s"an unexpected type representation reached the compiler backend while compiling ${ctx.compilationUnit}: $tp. " +
            "If possible, please file a bug on https://github.com/lampepfl/dotty/issues")

        tp match {
          case tp: ThisType if tp.cls == defn.ArrayClass => ObjectReference.asInstanceOf[ct.bTypes.ClassBType] // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
          case tp: ThisType                         => storage.getClassBTypeAndRegisterInnerClass(tp.cls.asInstanceOf[ct.int.Symbol])
          // case t: SingletonType                   => primitiveOrClassToBType(t.classSymbol)
          case t: SingletonType                     => typeToTypeKind(t.underlying)(ct)(storage)
          case t: RefinedType                       => typeToTypeKind(t.parent)(ct)(storage) //parents.map(_.toTypeKind(ct)(storage).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b))
        }
    }
  }

  object SelectBI extends DeconstructorCommon[Select] {

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

  object ThrowBI {
    var field: Throw = _
    def isEmpty: Boolean = field eq null
    def isDefined = !isEmpty
    def get: Tree = field.args.head
    def unapply(s: Throw): ThrowBI.type = {
      if (s.fun.symbol eq defn.throwMethod) {
        field = s
      } else {
        field = null
      }
      this
    }
  }

  object ArrayValueBI extends DeconstructorCommon[ArrayValue] {
    def _1: Type = field.tpe match {
      case JavaArrayType(elem) => elem
      case _ =>
        error(field.span, s"JavaSeqArray with type ${field.tpe} reached backend: $field")
        UnspecifiedErrorType
    }
    def _2: List[Tree] = field.elems
  }



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

  // Module symbols used in backend
  val StringModule: Symbol = symHelper(requiredClass[java.lang.String]).linkedClassOfClass
  val ScalaRunTimeModule: Symbol = requiredModule[scala.runtime.ScalaRunTime.type]


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
    primitiveCompilationUnits(sourceFileFor(ctx.compilationUnit))
  }

  def isCompilingArray = {
    sourceFileFor(ctx.compilationUnit) == "Array.scala"
  }
}

object DottyBackendInterface {
  val ExcludedForwarderFlags: Flags.FlagSet = {
    Flags.Specialized | Flags.Lifted | Flags.Protected | Flags.JavaStatic |
    Flags.Private | Flags.Macro
  }
}
