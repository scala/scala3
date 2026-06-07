package dotty.tools
package backend
package jvm

import scala.tools.asm
import scala.tools.asm.{AnnotationVisitor, ClassWriter, Opcodes}
import scala.collection.mutable
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.NameKinds.ExpandedName
import dotty.tools.dotc.core.Signature
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.transform.ElimErasedValueType
import dotty.tools.dotc.transform.Mixin
import dotty.tools.dotc.report
import tpd.*
import dotty.tools.dotc.config.ScalaSettingsProperties

/*
 *  Encapsulates functionality to convert Scala AST Trees into ASM ClassNodes.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
trait BCodeHelpers(val bTypeLoader: BTypeLoader, val bTypes: WellKnownBTypes) extends BCodeIdiomatic {

  // OK to cache because it won't change across Contexts
  private var cachedClassfileVersion: Int | Null = null
  protected def classfileVersion(using Context): Int =
    if cachedClassfileVersion == null then
      val releaseValue = Option(ctx.settings.javaOutputVersion.value).filter(_.nonEmpty)
      val targetValue = Option(ctx.settings.XuncheckedJavaOutputVersion.value).filter(_.nonEmpty)
      val target = (releaseValue, targetValue) match
        case (Some(release), None) => release
        case (None, Some(target)) => target
        case (Some(release), Some(_)) =>
          report.warning(s"The value of ${ctx.settings.XuncheckedJavaOutputVersion.name} was overridden by ${ctx.settings.javaOutputVersion.name}")
          release
        case (None, None) => ScalaSettingsProperties.supportedTargetVersions.min // least supported version by default
      // take advantage of the fact classfile versions are consecutive
      cachedClassfileVersion = target.toInt + (Opcodes.V17 - 17)
    cachedClassfileVersion.nn

  /*
   * can-multi-thread
   */
  def createJAttribute(name: String, b: Array[Byte], offset: Int, len: Int): asm.Attribute = {
    new asm.Attribute(name) {
      override def write(classWriter: ClassWriter, code: Array[Byte],
        codeLength: Int, maxStack: Int, maxLocals: Int): asm.ByteVector = {
        val byteVector = new asm.ByteVector(len)
        byteVector.putByteArray(b, offset, len)
        byteVector
      }
    }
  }

  /*
   * Custom attribute (JVMS 4.7.1) "ScalaSig" used as marker only
   * i.e., the pickle is contained in a custom annotation, see:
   *   (1) `addAnnotations()`,
   *   (2) SID # 10 (draft) - Storage of pickled Scala signatures in class files, http://www.scala-lang.org/sid/10
   *   (3) SID # 5 - Internals of Scala Annotations, http://www.scala-lang.org/sid/5
   * That annotation in turn is not related to the "java-generic-signature" (JVMS 4.7.9)
   * other than both ending up encoded as attributes (JVMS 4.7)
   * (with the caveat that the "ScalaSig" attribute is associated to some classes,
   * while the "Signature" attribute can be associated to classes, methods, and fields.)
   *
   */
  trait BCPickles {

    import dotty.tools.dotc.core.unpickleScala2.{ PickleFormat, PickleBuffer }

    private val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0, vp)
      vp.writeNat(PickleFormat.MajorVersion)
      vp.writeNat(PickleFormat.MinorVersion)
      vp.writeNat(0)
      vp
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerLocal(using Context) = {
      createJAttribute(nme.ScalaSignatureATTR.toString, versionPickle.bytes, 0, versionPickle.writeIndex)
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerForeign(using Context) = {
      createJAttribute(nme.ScalaATTR.toString, new Array[Byte](0), 0, 0)
    }
  } // end of trait BCPickles

  trait BCAnnotGen {
    // OK to cache these across Contexts, what they refer to won't change
    private var cachedAnnotationRetentionAttr: ClassSymbol | Null = null
    private var cachedAnnotationRetentionSource: TermSymbol | Null = null
    private var cachedAnnotationRetentionClass: TermSymbol | Null = null
    private var cachedAnnotationRetentionRuntime: TermSymbol | Null = null

    private def annotationRetentionAttr(using Context): ClassSymbol =
      if cachedAnnotationRetentionAttr eq null then
        cachedAnnotationRetentionAttr = requiredClass("java.lang.annotation.Retention")
      cachedAnnotationRetentionAttr.nn

    private def annotationRetentionSourceAttr(using Context): TermSymbol =
      if cachedAnnotationRetentionSource eq null then
        cachedAnnotationRetentionSource = requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("SOURCE")
      cachedAnnotationRetentionSource.nn

    private def annotationRetentionClassAttr(using Context): TermSymbol =
      if cachedAnnotationRetentionClass eq null then
        cachedAnnotationRetentionClass = requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("CLASS")
      cachedAnnotationRetentionClass.nn

    private def annotationRetentionRuntimeAttr(using Context): TermSymbol =
      if cachedAnnotationRetentionRuntime eq null then
        cachedAnnotationRetentionRuntime = requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("RUNTIME")
      cachedAnnotationRetentionRuntime.nn


    /*
     * must-single-thread
     */
    def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation])(using Context): Unit =
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val av = cw.visitAnnotation(bTypeLoader.bTypeFromType(typ).descriptor, isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }

    /*
     * must-single-thread
     */
    def emitAnnotations(mw: asm.MethodVisitor, annotations: List[Annotation])(using Context): Unit =
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val av = mw.visitAnnotation(bTypeLoader.bTypeFromType(typ).descriptor, isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }

    /*
     * must-single-thread
     */
    def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation])(using Context): Unit =
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val av = fw.visitAnnotation(bTypeLoader.bTypeFromType(typ).descriptor, isRuntimeVisible(annot))
        emitAssocs(av, assocs)
      }

    /*
     * must-single-thread
     */
    def emitParamNames(jmethod: asm.MethodVisitor, params: List[Symbol])(using Context): Unit =
      for param <- params do
        var access = asm.Opcodes.ACC_FINAL
        if param.is(Artifact) then access |= asm.Opcodes.ACC_SYNTHETIC
        jmethod.visitParameter(param.name.mangledString, access)

    /*
     * must-single-thread
     */
    def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[Annotation]])(using Context): Unit =
      val annotationss = pannotss.map(_.filter(shouldEmitAnnotation))
      if (annotationss.forall(_.isEmpty)) return
      for ((annots, idx) <- annotationss.zipWithIndex; annot <- annots) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, bTypeLoader.bTypeFromType(typ).descriptor, isRuntimeVisible(annot))
        emitAssocs(pannVisitor, assocs)
      }


    private def shouldEmitAnnotation(annot: Annotation)(using Context): Boolean = {
      annot.symbol.is(JavaDefined) &&
        retentionPolicyOf(annot) != annotationRetentionSourceAttr
    }

    private def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, Object)])(using Context): Unit = {
      for ((name, value) <- assocs)
        emitArgument(av, name.mangledString, value.asInstanceOf[Tree])
      av.visitEnd()
    }

    private def emitArgument(av:   AnnotationVisitor,
                             name: String | Null,
                             arg:  Tree)(using Context): Unit = {
      val narg = normalizeArgument(arg)
      // Transformation phases are not run on annotation trees, so we need to run
      // `constToLiteral` at this point.
      val t = atPhase(erasurePhase)(constToLiteral(narg))
      t match {
        case Literal(const @ Constant(_)) =>
          const.tag match {
            case BooleanTag | ByteTag | ShortTag | CharTag | IntTag | LongTag | FloatTag | DoubleTag => av.visit(name, const.value)
            case StringTag => av.visit(name, const.stringValue)
            case ClazzTag => av.visit(name, bTypeLoader.bTypeFromType(TypeErasure.erasure(const.typeValue)).toASMType)
          }
        case Ident(nme.WILDCARD) =>
          // An underscore argument indicates that we want to use the default value for this parameter, so do not emit anything
        case t: tpd.RefTree if t.symbol.owner.linkedClass.isAllOf(JavaEnum) =>
          val edesc = bTypeLoader.bTypeFromType(t.tpe).descriptor // the class descriptor of the enumeration class.
          val evalue = t.symbol.javaSimpleName // value the actual enumeration value.
          av.visitEnum(name, edesc, evalue)
        // Handle final val aliases to Java enum values.
        // Check if the symbol's pre-erasure type was a singleton of a Java enum value.
        case t: tpd.RefTree if atPhase(erasurePhase) {
          t.symbol.info.finalResultType match
            case tr: TermRef => tr.termSymbol.owner.linkedClass.isAllOf(JavaEnum)
            case _ => false
        } =>
          val enumRef = atPhase(erasurePhase)(t.symbol.info.finalResultType.asInstanceOf[TermRef])
          val edesc = bTypeLoader.bTypeFromType(enumRef).descriptor
          val evalue = enumRef.termSymbol.javaSimpleName
          av.visitEnum(name, edesc, evalue)
        case t: SeqLiteral =>
          val arrAnnotV: AnnotationVisitor = av.visitArray(name)
          for (arg <- t.elems) { emitArgument(arrAnnotV, null, arg) }
          arrAnnotV.visitEnd()

        case Apply(fun, args) if fun.symbol == defn.ArrayClass.primaryConstructor ||
          toDenot(fun.symbol).owner == defn.ArrayClass.linkedClass && fun.symbol.name == nme.apply =>
          val arrAnnotV: AnnotationVisitor = av.visitArray(name)

          val actualArgs = if (fun.tpe.isImplicitMethod) {
            // generic array method, need to get implicit argument out of the way
            fun.asInstanceOf[Apply].args
          } else args

          val flatArgs = actualArgs.flatMap { arg =>
            normalizeArgument(arg) match {
              case t: tpd.SeqLiteral => t.elems
              case e => List(e)
            }
          }
          for arg <- flatArgs do
            emitArgument(arrAnnotV, null, arg)
          arrAnnotV.visitEnd()
  /*
        case sb @ ScalaSigBytes(bytes) =>
          // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
          // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
          if (sb.fitsInOneString) {
            av.visit(name, BCodeUtils.strEncode(sb))
          } else {
            val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
            for(arg <- BCodeUtils.arrEncode(sb)) { arrAnnotV.visit(name, arg) }
            arrAnnotV.visitEnd()
          }          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.
  */
        case t @ Apply(constr, args) if t.tpe.classSymbol.is(JavaAnnotation) =>
          val typ = t.tpe.classSymbol.denot.info
          val assocs = assocsFromApply(t)
          val desc = bTypeLoader.bTypeFromType(typ).descriptor // the class descriptor of the nested annotation class
          val nestedVisitor = av.visitAnnotation(name, desc)
          emitAssocs(nestedVisitor, assocs)

        case Inlined(_, _, expansion) =>
          emitArgument(av, name, arg = expansion)

        case t =>
          report.error(em"Annotation argument is not a constant", t.sourcePos)
      }
    }

    private def normalizeArgument(arg: Tree): Tree = arg match {
      case Trees.NamedArg(_, arg1) => normalizeArgument(arg1)
      case Trees.Typed(arg1, _) => normalizeArgument(arg1)
      case _ => arg
    }

    private def isRuntimeVisible(annot: Annotation)(using Context): Boolean =
      if (toDenot(annot.tree.tpe.typeSymbol).hasAnnotation(annotationRetentionAttr))
        retentionPolicyOf(annot) == annotationRetentionRuntimeAttr
      else {
        // SI-8926: if the annotation class symbol doesn't have a @RetentionPolicy annotation, the
        // annotation is emitted with visibility `RUNTIME`
        // dotty bug: #389
        true
      }

    private def retentionPolicyOf(annot: Annotation)(using Context): Symbol =
      annot.tree.tpe.typeSymbol.getAnnotation(annotationRetentionAttr).
        flatMap(_.argument(0).map(_.tpe.termSymbol)).getOrElse(annotationRetentionClassAttr)

    private def assocsFromApply(tree: Tree)(using Context): List[(Name, Tree)] = {
      tree match {
        case Block(_, expr) => assocsFromApply(expr)
        case Apply(fun, args) =>
          fun.tpe.widen match {
            case MethodType(names) =>
              names.zip(args).filter {
                case (_, t: tpd.Ident) if t.tpe.normalizedPrefix eq NoPrefix => false
                case _ => true
              }
          }
      }
    }
  } // end of trait BCAnnotGen

  trait BCJGenSigGen {

    /**
     * Generates the generic signature for `sym` before erasure.
     *
     * @param sym   The symbol for which to generate a signature.
     * @param owner The owner of `sym`.
     * @param descriptor The descriptor of the symbol; the signature is unnecessary if they are equal.
     * @return The generic signature of `sym` before erasure, as specified in the Java Virtual
     *         Machine Specification, §4.3.4, or `null` if `sym` doesn't need a generic signature.
     * @see https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3.4
     */
    def getGenericSignature(sym: Symbol, owner: Symbol, descriptor: String | Null)(using Context): String | Null = {
      atPhase(erasurePhase) {
        // Finding the member's type is nontrivial because of erasure and how it interacts with other phases.
        def computeMemberType(): Type = {
          // Mixins are resolved _after_ erasure, so we cannot simply ask for "the information before erasure" for these,
          // since that information never existed.
          // Thus, we first check if the symbol was specifically marked as having generic information,
          mixinPhase.asInstanceOf[Mixin].mixinGenericInfos.get(sym) match
            // and if so, we use it.
            case Some(genericInfo) => return genericInfo
            case _ => ()

          // Methods are straightforward.
          if sym.is(Method) then
            return sym.denot.info

          // Fields have two special cases:
          if sym.isField then
            // we must use the getter if entered after erasure at memoize, see tests/generic-java-signatures/17069.scala for an example
            if sym.denot.validFor.firstPhaseId > erasurePhase.id then
              if sym.getter.exists then
                return sym.getter.denot.info.resultType

              // there might be a getter created after erasure by the mixin phase,
              // and if so we must use the information that the mixin phase stored for it.
              val mixinGetter = atPhase(mixinPhase.next) { sym.getter }
              if mixinGetter.exists then mixinPhase.asInstanceOf[Mixin].mixinGenericInfos.get(mixinGetter) match
                case Some(ExprType(genericInfo)) => return genericInfo // since we're looking for the getter, we get an ExprType
                case _ => ()

          owner.denot.thisType.memberInfo(sym)
        }

        if ctx.base.settings.XnoGenericSig.value then null
        else
          val genSig = getGenericSignatureHelper(sym, owner, computeMemberType())
          if genSig == null || (descriptor != null && descriptor.contentEquals(genSig)) then null
          else genSig.toString
      }
    }

  } // end of trait BCJGenSigGen

  trait BCForwardersGen extends BCAnnotGen with BCJGenSigGen {

    /* Add a forwarder for method m. Used only from addForwarders().
     *
     * must-single-thread
     */
    private def addForwarder(jclass: asm.ClassVisitor, module: Symbol, m: Symbol, isSynthetic: Boolean)(using Context): Unit = {
      val moduleName     = bTypeLoader.classBTypeFromSymbol(module).internalName
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes: List[BType] = methodInfo.firstParamTypes.map(bTypeLoader.bTypeFromType)
      // val paramNames     = 0 until paramJavaTypes.length.map("x_" + _)

      /* Forwarders must not be marked final,
       *  as the JVM will not allow redefinition of a final static method,
       *  and we don't know what classes might be subclassing the companion class.  See SI-4827.
       */
      // TODO: evaluate the other flags we might be dropping on the floor here.
      val flags = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | (
        if (m.is(JavaVarargs)) asm.Opcodes.ACC_VARARGS else 0
      ) | (
        if (isSynthetic) asm.Opcodes.ACC_SYNTHETIC else 0
      )

      // TODO needed? for(ann <- m.annotations) { ann.symbol.initialize }
      val jReturnType = bTypeLoader.bTypeFromType(methodInfo.resultType)
      val mdesc = MethodBType(paramJavaTypes, jReturnType).descriptor
      val jgensig = getStaticForwarderGenericSignature(m, module, mdesc)
      val (throws, others) = m.annotations.partition(_.symbol eq defn.ThrowsAnnot)
      val thrownExceptions: List[String] = getExceptions(throws)

      val mirrorMethodName = m.javaSimpleName
      val lengthOk = if jgensig ne null then BCodeUtils.checkConstantStringLength(jgensig)
                                        else BCodeUtils.checkConstantStringLength(mirrorMethodName, mdesc)
      if !lengthOk then
        report.error("Mirror method signature is too long for the JVM", m.srcPos)
        return
      val mirrorMethod: asm.MethodVisitor = jclass.visitMethod(
        flags,
        mirrorMethodName,
        mdesc,
        jgensig,
        mkArrayS(thrownExceptions)
      )

      emitAnnotations(mirrorMethod, others)
      val params: List[Symbol] = Nil // backend uses this to emit annotations on parameter lists of forwarders
      // to static methods of companion class
      // Old assumption: in Dotty this link does not exists: there is no way to get from method type
      // to inner symbols of DefDef
      // TODO: now we have paramSymss and could use it here.
      emitParamAnnotations(mirrorMethod, params.map(_.annotations))

      mirrorMethod.visitCode()

      mirrorMethod.visitFieldInsn(asm.Opcodes.GETSTATIC, moduleName, str.MODULE_INSTANCE_FIELD, bTypeLoader.classBTypeFromSymbol(module).descriptor)

      var index = 0
      for(jparamType <- paramJavaTypes) {
        mirrorMethod.visitVarInsn(jparamType.typedOpcode(asm.Opcodes.ILOAD), index)
        assert(!jparamType.isInstanceOf[MethodBType], jparamType)
        index += jparamType.size
      }

      mirrorMethod.visitMethodInsn(asm.Opcodes.INVOKEVIRTUAL, moduleName, mirrorMethodName, bTypeLoader.methodBTypeFromSymbol(m).descriptor, false)
      mirrorMethod.visitInsn(jReturnType.typedOpcode(asm.Opcodes.IRETURN))

      mirrorMethod.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      mirrorMethod.visitEnd()

    }

    /* Add forwarders for all methods defined in `module` that don't conflict
     *  with methods in the companion class of `module`. A conflict arises when
     *  a method with the same name is defined both in a class and its companion object:
     *  method signature is not taken into account.
     *
     * must-single-thread
     */
    def addForwarders(jclass: asm.ClassVisitor, jclassName: String, moduleClass: Symbol)(using Context): Unit = {
      assert(moduleClass.is(ModuleClass), moduleClass)
      report.debuglog(s"Dumping mirror class for object: $moduleClass")

      val linkedClass  = moduleClass.companionClass
      lazy val conflictingNames: Set[Name] =
        linkedClass.info.allMembers.collect { case d if d.name.isTermName => d.name }.toSet
      report.debuglog(s"Potentially conflicting names for forwarders: $conflictingNames")

      for (m0 <- sortedMembersBasedOnFlags(moduleClass.info, required = Method, excluded = ExcludedForwarder)) {
        val m = if (m0.isOneOf(Bridge | MixedIn)) m0.nextOverriddenSymbol else m0
        if (m == NoSymbol)
          report.log(s"$m0 is a bridge method that overrides nothing, something went wrong in a previous phase.")
        else if (m.isType || m.is(Deferred) || (m.owner eq defn.ObjectClass) || m.isConstructor || m.name.is(ExpandedName))
          report.debuglog(s"No forwarder for '$m' from $jclassName to '$moduleClass'")
        else if (conflictingNames(m.name))
          report.log(s"No forwarder for $m due to conflict with ${linkedClass.info.member(m.name)}")
        else if (m.accessBoundary(defn.RootClass) ne defn.RootClass)
          report.log(s"No forwarder for non-public member $m")
        else {
          report.log(s"Adding static forwarder for '$m' from $jclassName to '$moduleClass'")
          // It would be simpler to not generate forwarders for these methods,
          // but that wouldn't be binary-compatible with Scala 3.0.0, so instead
          // we generate ACC_SYNTHETIC forwarders so Java compilers ignore them.
          val isSynthetic =
            m0.name.is(NameKinds.SyntheticSetterName) ||
            m0.is(Bridge)
          addForwarder(jclass, moduleClass, m, isSynthetic)
        }
      }
    }

    /** The members of this type that have all of `required` flags but none of `excluded` flags set.
     *  The members are sorted by name and signature to guarantee a stable ordering.
     */
    private def sortedMembersBasedOnFlags(tp: Type, required: Flag, excluded: FlagSet)(using Context): List[Symbol] = {
      // The output of `memberNames` is a Set, sort it to guarantee a stable ordering.
      val names = tp.memberNames(takeAllFilter).toSeq.sorted
      val buffer = mutable.ListBuffer[Symbol]()
      names.foreach { name =>
        buffer ++= tp.memberBasedOnFlags(name, required, excluded)
          .alternatives.sortBy(_.signature)(using Signature.lexicographicOrdering).map(_.symbol)
      }
      buffer.toList
    }

    /*
     * Quoting from JVMS 4.7.5 The Exceptions Attribute
     *   "The Exceptions attribute indicates which checked exceptions a method may throw.
     *    There may be at most one Exceptions attribute in each method_info structure."
     *
     * The contents of that attribute are determined by the `String[] exceptions` argument to ASM's ClassVisitor.visitMethod()
     * This method returns such list of internal names.
     *
     * must-single-thread
     */
    def getExceptions(excs: List[Annotation])(using Context): List[String] = {
      for (case ThrownException(exc) <- excs.distinct)
      yield bTypeLoader.classBTypeFromSymbol(TypeErasure.erasure(exc).classSymbol).internalName
    }
  } // end of trait BCForwardersGen

  trait BCClassGen {

    // Used as threshold above which a tableswitch bytecode instruction is preferred over a lookupswitch.
    // There's a space tradeoff between these multi-branch instructions (details in the JVM spec).
    // The particular value in use for `MIN_SWITCH_DENSITY` reflects a heuristic.
    val MIN_SWITCH_DENSITY = 0.7

    /*
     *  Add public static final field serialVersionUID with value `id`
     *
     *  can-multi-thread
     */
    def addSerialVUID(id: Long, jclass: asm.ClassVisitor): Unit = {
      // add static serialVersionUID field if `clasz` annotated with `@SerialVersionUID(uid: Long)`
      jclass.visitField(
        asm.Opcodes.ACC_PRIVATE | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL,
        "serialVersionUID",
        "J",
        null, // no java-generic-signature
        java.lang.Long.valueOf(id)
      ).visitEnd()
    }
  } // end of trait BCClassGen

  /* functionality for building plain and mirror classes */
  abstract class JCommonBuilder
    extends BCAnnotGen
    with    BCForwardersGen
    with    BCPickles { }

  /* builder of mirror classes */
  class JMirrorBuilder extends JCommonBuilder {

    /* Generate a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     *
     *  must-single-thread
     */
    def genMirrorClass(moduleClass: Symbol)(using Context): asm.tree.ClassNode = {
      assert(moduleClass.is(ModuleClass))
      assert(moduleClass.companionClass == NoSymbol, moduleClass)
      val bType      = bTypeLoader.mirrorClassBTypeFromSymbol(moduleClass)
      val moduleName = bTypeLoader.classBTypeFromSymbol(moduleClass).internalName
      val mirrorName = bType.internalName
      val mirrorClass = new asm.tree.ClassNode
      if !BCodeUtils.checkConstantStringLength(mirrorName) then
        report.error("Mirror class name is too long for the JVM", moduleClass.srcPos)
        return mirrorClass // not filled, but we cannot create it, and we just reported an error
      mirrorClass.visit(
        classfileVersion,
        bType.info.flags,
        mirrorName,
        null /* no java-generic-signature */,
        bTypes.ObjectRef.internalName,
        EMPTY_STRING_ARRAY
      )

      if (BackendUtils.emitSource) {
        mirrorClass.visitSource("" + ctx.compilationUnit.source.file.name, null /* SourceDebugExtension */)
      }

      val ssa = None // getAnnotPickle(mirrorName, if (moduleClass.is(Module)) moduleClass.companionClass else moduleClass.companionModule)
      mirrorClass.visitAttribute(if (ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(mirrorClass, moduleClass.annotations ++ ssa)

      addForwarders(mirrorClass, mirrorName, moduleClass)
      mirrorClass.visitEnd()

      moduleClass.name // this side effect is necessary, really.

      mirrorClass
    }

  } // end of class JMirrorBuilder

  private def getGenericSignatureHelper(sym: Symbol, owner: Symbol, memberTpe: Type)(using Context): java.lang.StringBuilder | Null = {
    val erasedTypeSym = TypeErasure.fullErasure(sym.denot.info).typeSymbol
    if (erasedTypeSym.isPrimitiveValueClass) {
      // Suppress signatures for symbols whose types erase in the end to primitive
      // value types. This is needed to fix #7416.
      null
    } else {
      val jsOpt = GenericSignatures.javaSig(sym, memberTpe)
      if (jsOpt != null && ctx.settings.XverifySignatures.value) {
        verifySignature(sym, jsOpt.toString)
      }
      jsOpt
    }
  }

  private def verifySignature(sym: Symbol, sig: String)(using Context): Unit = {
    import scala.tools.asm.util.CheckClassAdapter
    def wrap(body: => Unit): Unit = {
      try body
      catch case ex: Exception =>
        report.error(
          em"""|compiler bug: created invalid generic signature for $sym in ${sym.denot.owner.showFullName}
               |signature: $sig
               |if this is reproducible, please report bug at https://github.com/scala/scala3/issues
             """, sym.sourcePos)
        throw ex
    }

    wrap {
      if (sym.is(Method)) {
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

  private def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol, descriptor: String | Null)(using Context): String | Null = {
    // scala/bug#3452 Static forwarder generation uses the same erased signature as the method if forwards to.
    // By rights, it should use the signature as-seen-from the module class, and add suitable
    // primitive and value-class boxing/unboxing.
    // But for now, just like we did in mixin, we just avoid writing a wrong generic signature
    // (one that doesn't erase to the actual signature). See run/t3452b for a test case.

    if !ctx.base.settings.XnoGenericSig.value then
      val memberTpe = atPhase(erasurePhase) { moduleClass.denot.thisType.memberInfo(sym) }
      val erasedMemberType = ElimErasedValueType.elimEVT(TypeErasure.transformInfo(sym, memberTpe))
      if (erasedMemberType =:= sym.denot.info)
        val gensig = getGenericSignatureHelper(sym, moduleClass, memberTpe)
        if gensig == null || (descriptor != null && descriptor.contentEquals(gensig)) then null
        else gensig.toString
      else null
    else null
  }
}

object BCodeHelpers {

  class InvokeStyle(val style: Int) extends AnyVal {
    import InvokeStyle.*
    def isVirtual: Boolean = this == Virtual
    def isStatic : Boolean = this == Static
    def isSpecial: Boolean = this == Special
    def isSuper  : Boolean = this == Super

    def hasInstance: Boolean = this != Static
  }

  object InvokeStyle {
    val Virtual = new InvokeStyle(0) // InvokeVirtual or InvokeInterface
    val Static  = new InvokeStyle(1) // InvokeStatic
    val Special = new InvokeStyle(2) // InvokeSpecial (private methods, constructors)
    val Super   = new InvokeStyle(3) // InvokeSpecial (super calls)
  }

  /** An attachment on Apply nodes indicating that it should be compiled with
   *  `invokespecial` instead of `invokevirtual`. This is used for static
   *  forwarders.
   *  See BCodeSkelBuilder.makeStaticForwarder for more details.
   */
  val UseInvokeSpecial = new dotc.util.Property.Key[Unit]

  /**
   * Valid flags for InnerClass attribute entry.
   * See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.6
   */
  val INNER_CLASSES_FLAGS = {
    asm.Opcodes.ACC_PUBLIC   | asm.Opcodes.ACC_PRIVATE   | asm.Opcodes.ACC_PROTECTED  |
      asm.Opcodes.ACC_STATIC   | asm.Opcodes.ACC_FINAL     | asm.Opcodes.ACC_INTERFACE  |
      asm.Opcodes.ACC_ABSTRACT | asm.Opcodes.ACC_SYNTHETIC | asm.Opcodes.ACC_ANNOTATION |
      asm.Opcodes.ACC_ENUM
  }

}
