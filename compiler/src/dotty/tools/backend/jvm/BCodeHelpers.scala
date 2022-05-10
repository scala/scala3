package dotty.tools
package backend
package jvm

import scala.language.unsafeNulls

import scala.annotation.threadUnsafe
import scala.tools.asm
import scala.tools.asm.AnnotationVisitor
import scala.tools.asm.ClassWriter
import scala.collection.mutable

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.NameKinds.ExpandedName
import dotty.tools.dotc.core.Signature
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.transform.GenericSignatures
import dotty.tools.dotc.transform.ElimErasedValueType
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.report

import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions

/*
 *  Traits encapsulating functionality to convert Scala AST Trees into ASM ClassNodes.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
trait BCodeHelpers extends BCodeIdiomatic with BytecodeWriters {
  // for some reason singleton types aren't allowed in constructor calls. will need several casts in code to enforce

  //import global._
  //import bTypes._
  //import coreBTypes._
  import bTypes._
  import tpd._
  import coreBTypes._
  import int.{_, given}
  import DottyBackendInterface._

  def ScalaATTRName: String = "Scala"
  def ScalaSignatureATTRName: String = "ScalaSig"

  @threadUnsafe lazy val AnnotationRetentionAttr: ClassSymbol = requiredClass("java.lang.annotation.Retention")
  @threadUnsafe lazy val AnnotationRetentionSourceAttr: TermSymbol = requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("SOURCE")
  @threadUnsafe lazy val AnnotationRetentionClassAttr: TermSymbol = requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("CLASS")
  @threadUnsafe lazy val AnnotationRetentionRuntimeAttr: TermSymbol = requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("RUNTIME")
  @threadUnsafe lazy val JavaAnnotationClass: ClassSymbol = requiredClass("java.lang.annotation.Annotation")

  val bCodeAsmCommon: BCodeAsmCommon[int.type] = new BCodeAsmCommon(int)

  /*
   * must-single-thread
   */
  def getFileForClassfile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    getFile(base, clsName, suffix)
  }

  /*
   * must-single-thread
   */
  def getOutFolder(csym: Symbol, cName: String): AbstractFile = {
    try {
      outputDirectory
    } catch {
      case ex: Throwable =>
        report.error(s"Couldn't create file for class $cName\n${ex.getMessage}", ctx.source.atSpan(csym.span))
        null
    }
  }

  final def traitSuperAccessorName(sym: Symbol): String = {
    val nameString = sym.javaSimpleName.toString
    if (sym.name == nme.TRAIT_CONSTRUCTOR) nameString
    else nameString + "$"
  }

  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://issues.scala-lang.org/browse/SI-3872
  // -----------------------------------------------------------------------------------------

  /*  An `asm.ClassWriter` that uses `jvmWiseLUB()`
   *  The internal name of the least common ancestor of the types given by inameA and inameB.
   *  It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
   */
  final class CClassWriter(flags: Int) extends asm.ClassWriter(flags) {

    /**
     * This method is thread-safe: it depends only on the BTypes component, which does not depend
     * on global. TODO @lry move to a different place where no global is in scope, on bTypes.
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      val a = classBTypeFromInternalName(inameA)
      val b = classBTypeFromInternalName(inameB)
      val lub = a.jvmWiseLUB(b)
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }
  }

  /*
   * must-single-thread
   */
  def initBytecodeWriter(): BytecodeWriter = {
    (None: Option[AbstractFile] /*getSingleOutput*/) match { // todo: implement
      case Some(f) if f.hasExtension("jar") =>
        new DirectToJarfileWriter(f.file)
      case _ =>
        factoryNonJarBytecodeWriter()
    }
  }

  /*
   * Populates the InnerClasses JVM attribute with `refedInnerClasses`. See also the doc on inner
   * classes in BTypes.scala.
   *
   * `refedInnerClasses` may contain duplicates, need not contain the enclosing inner classes of
   * each inner class it lists (those are looked up and included).
   *
   * This method serializes in the InnerClasses JVM attribute in an appropriate order, 
   * not necessarily that given by `refedInnerClasses`.
   *
   * can-multi-thread
   */
  final def addInnerClasses(jclass: asm.ClassVisitor, declaredInnerClasses: List[ClassBType], refedInnerClasses: List[ClassBType]): Unit = {
    // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    val allNestedClasses = new mutable.TreeSet[ClassBType]()(Ordering.by(_.internalName))
    allNestedClasses ++= declaredInnerClasses
    refedInnerClasses.foreach(allNestedClasses ++= _.enclosingNestedClassesChain)
    for nestedClass <- allNestedClasses
    do {
      // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
      val Some(e) = nestedClass.innerClassAttributeEntry
      jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
    }
  }

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

    val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0, vp)
      vp writeNat PickleFormat.MajorVersion
      vp writeNat PickleFormat.MinorVersion
      vp writeNat 0
      vp
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerLocal = {
      createJAttribute(ScalaSignatureATTRName, versionPickle.bytes, 0, versionPickle.writeIndex)
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerForeign = {
      createJAttribute(ScalaATTRName, new Array[Byte](0), 0, 0)
    }
  } // end of trait BCPickles

  trait BCInnerClassGen {

    def debugLevel = 3 // 0 -> no debug info; 1-> filename; 2-> lines; 3-> varnames

    final val emitSource = debugLevel >= 1
    final val emitLines  = debugLevel >= 2
    final val emitVars   = debugLevel >= 3

    /**
     * The class internal name for a given class symbol.
     */
    final def internalName(sym: Symbol): String = {
      // For each java class, the scala compiler creates a class and a module (thus a module class).
      // If the `sym` is a java module class, we use the java class instead. This ensures that the
      // ClassBType is created from the main class (instead of the module class).
      // The two symbols have the same name, so the resulting internalName is the same.
      val classSym = if (sym.is(JavaDefined) && sym.is(ModuleClass)) sym.linkedClass else sym
      getClassBType(classSym).internalName
    }

    private def assertClassNotArray(sym: Symbol): Unit = {
      assert(sym.isClass, sym)
      assert(sym != defn.ArrayClass || compilingArray, sym)
    }

    private def assertClassNotArrayNotPrimitive(sym: Symbol): Unit = {
      assertClassNotArray(sym)
      assert(!primitiveTypeMap.contains(sym) || isCompilingPrimitive, sym)
    }

    /**
     * The ClassBType for a class symbol.
     *
     * The class symbol scala.Nothing is mapped to the class scala.runtime.Nothing$. Similarly,
     * scala.Null is mapped to scala.runtime.Null$. This is because there exist no class files
     * for the Nothing / Null. If used for example as a parameter type, we use the runtime classes
     * in the classfile method signature.
     *
     * Note that the referenced class symbol may be an implementation class. For example when
     * compiling a mixed-in method that forwards to the static method in the implementation class,
     * the class descriptor of the receiver (the implementation class) is obtained by creating the
     * ClassBType.
     */
    final def getClassBType(sym: Symbol): ClassBType = {
      assertClassNotArrayNotPrimitive(sym)

      if (sym == defn.NothingClass) srNothingRef
      else if (sym == defn.NullClass) srNullRef
      else classBTypeFromSymbol(sym)
    }

    /*
     * must-single-thread
     */
    final def asmMethodType(msym: Symbol): MethodBType = {
      assert(msym.is(Method), s"not a method-symbol: $msym")
      val resT: BType =
        if (msym.isClassConstructor || msym.isConstructor) UNIT
        else toTypeKind(msym.info.resultType)
      MethodBType(msym.info.firstParamTypes map toTypeKind, resT)
    }

    /**
     * The jvm descriptor of a type.
     */
    final def typeDescriptor(t: Type): String = { toTypeKind(t).descriptor   }

    /**
     * The jvm descriptor for a symbol.
     */
    final def symDescriptor(sym: Symbol): String = getClassBType(sym).descriptor

    final def toTypeKind(tp: Type): BType = typeToTypeKind(tp)(BCodeHelpers.this)(this)

  } // end of trait BCInnerClassGen

  trait BCAnnotGen extends BCInnerClassGen {

    /*
     * must-single-thread
     */
    def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation]): Unit =
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val av = cw.visitAnnotation(typeDescriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs, BCodeHelpers.this)(this)
      }

    /*
     * must-single-thread
     */
    def emitAnnotations(mw: asm.MethodVisitor, annotations: List[Annotation]): Unit =
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val av = mw.visitAnnotation(typeDescriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs, BCodeHelpers.this)(this)
      }

    /*
     * must-single-thread
     */
    def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation]): Unit =
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val av = fw.visitAnnotation(typeDescriptor(typ), isRuntimeVisible(annot))
        emitAssocs(av, assocs, BCodeHelpers.this)(this)
      }

    /*
     * must-single-thread
     */
    def emitParamNames(jmethod: asm.MethodVisitor, params: List[Symbol]) =
      for param <- params do
        var access = asm.Opcodes.ACC_FINAL
        if param.is(Artifact) then access |= asm.Opcodes.ACC_SYNTHETIC
        jmethod.visitParameter(param.name.mangledString, access)

    /*
     * must-single-thread
     */
    def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[Annotation]]): Unit =
      val annotationss = pannotss map (_ filter shouldEmitAnnotation)
      if (annotationss forall (_.isEmpty)) return
      for ((annots, idx) <- annotationss.zipWithIndex;
        annot <- annots) {
        val typ = annot.tree.tpe
        val assocs = assocsFromApply(annot.tree)
        val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, typeDescriptor(typ.asInstanceOf[Type]), isRuntimeVisible(annot))
        emitAssocs(pannVisitor, assocs, BCodeHelpers.this)(this)
      }


    private def shouldEmitAnnotation(annot: Annotation): Boolean = {
      annot.symbol.is(JavaDefined) &&
        retentionPolicyOf(annot) != AnnotationRetentionSourceAttr
    }

    private def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, Object)], bcodeStore: BCodeHelpers)
        (innerClasesStore: bcodeStore.BCInnerClassGen) = {
      for ((name, value) <- assocs)
        emitArgument(av, name.mangledString, value.asInstanceOf[Tree], bcodeStore)(innerClasesStore)
      av.visitEnd()
    }

    private def emitArgument(av:   AnnotationVisitor,
                           name: String,
                           arg:  Tree, bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
      val narg = normalizeArgument(arg)
      // Transformation phases are not run on annotation trees, so we need to run
      // `constToLiteral` at this point.
      val t = atPhase(erasurePhase)(constToLiteral(narg))
      t match {
        case Literal(const @ Constant(_)) =>
          const.tag match {
            case BooleanTag | ByteTag | ShortTag | CharTag | IntTag | LongTag | FloatTag | DoubleTag => av.visit(name, const.value)
            case StringTag =>
              assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
              av.visit(name, const.stringValue) // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
            case ClazzTag => av.visit(name, typeToTypeKind(TypeErasure.erasure(const.typeValue))(bcodeStore)(innerClasesStore).toASMType)
          }
        case Ident(nme.WILDCARD) =>
          // An underscore argument indicates that we want to use the default value for this parameter, so do not emit anything
        case t: tpd.RefTree if t.symbol.owner.linkedClass.isAllOf(JavaEnumTrait) =>
          val edesc = innerClasesStore.typeDescriptor(t.tpe) // the class descriptor of the enumeration class.
          val evalue = t.symbol.javaSimpleName // value the actual enumeration value.
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
          val desc = innerClasesStore.typeDescriptor(typ) // the class descriptor of the nested annotation class
          val nestedVisitor = av.visitAnnotation(name, desc)
          emitAssocs(nestedVisitor, assocs, bcodeStore)(innerClasesStore)

        case t =>
          report.error(ex"Annotation argument is not a constant", t.sourcePos)
      }
    }

    private def normalizeArgument(arg: Tree): Tree = arg match {
      case Trees.NamedArg(_, arg1) => normalizeArgument(arg1)
      case Trees.Typed(arg1, _) => normalizeArgument(arg1)
      case _ => arg
    }

    private def isRuntimeVisible(annot: Annotation): Boolean =
      if (toDenot(annot.tree.tpe.typeSymbol).hasAnnotation(AnnotationRetentionAttr))
        retentionPolicyOf(annot) == AnnotationRetentionRuntimeAttr
      else {
        // SI-8926: if the annotation class symbol doesn't have a @RetentionPolicy annotation, the
        // annotation is emitted with visibility `RUNTIME`
        // dotty bug: #389
        true
      }

    private def retentionPolicyOf(annot: Annotation): Symbol =
      annot.tree.tpe.typeSymbol.getAnnotation(AnnotationRetentionAttr).
        flatMap(_.argument(0).map(_.tpe.termSymbol)).getOrElse(AnnotationRetentionClassAttr)

    private def assocsFromApply(tree: Tree): List[(Name, Tree)] = {
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
  } // end of trait BCAnnotGen

  trait BCJGenSigGen {
    import int.given

    def getCurrentCUnit(): CompilationUnit

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
      atPhase(erasurePhase) {
        val memberTpe =
          if (sym.is(Method)) sym.denot.info
          else owner.denot.thisType.memberInfo(sym)
        getGenericSignatureHelper(sym, owner, memberTpe).orNull
      }
    }

  } // end of trait BCJGenSigGen

  trait BCForwardersGen extends BCAnnotGen with BCJGenSigGen {

    /* Add a forwarder for method m. Used only from addForwarders().
     *
     * must-single-thread
     */
    private def addForwarder(jclass: asm.ClassVisitor, module: Symbol, m: Symbol, isSynthetic: Boolean): Unit = {
      val moduleName     = internalName(module)
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes: List[BType] = methodInfo.firstParamTypes map toTypeKind
      // val paramNames     = 0 until paramJavaTypes.length map ("x_" + _)

      /* Forwarders must not be marked final,
       *  as the JVM will not allow redefinition of a final static method,
       *  and we don't know what classes might be subclassing the companion class.  See SI-4827.
       */
      // TODO: evaluate the other flags we might be dropping on the floor here.
      val flags = GenBCodeOps.PublicStatic | (
        if (m.is(JavaVarargs)) asm.Opcodes.ACC_VARARGS else 0
      ) | (
        if (isSynthetic) asm.Opcodes.ACC_SYNTHETIC else 0
      )

      // TODO needed? for(ann <- m.annotations) { ann.symbol.initialize }
      val jgensig = getStaticForwarderGenericSignature(m, module)
      val (throws, others) = m.annotations.partition(_.symbol eq defn.ThrowsAnnot)
      val thrownExceptions: List[String] = getExceptions(throws)

      val jReturnType = toTypeKind(methodInfo.resultType)
      val mdesc = MethodBType(paramJavaTypes, jReturnType).descriptor
      val mirrorMethodName = m.javaSimpleName
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

      mirrorMethod.visitFieldInsn(asm.Opcodes.GETSTATIC, moduleName, str.MODULE_INSTANCE_FIELD, symDescriptor(module))

      var index = 0
      for(jparamType <- paramJavaTypes) {
        mirrorMethod.visitVarInsn(jparamType.typedOpcode(asm.Opcodes.ILOAD), index)
        assert(!jparamType.isInstanceOf[MethodBType], jparamType)
        index += jparamType.size
      }

      mirrorMethod.visitMethodInsn(asm.Opcodes.INVOKEVIRTUAL, moduleName, mirrorMethodName, asmMethodType(m).descriptor, false)
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
    def addForwarders(jclass: asm.ClassVisitor, jclassName: String, moduleClass: Symbol): Unit = {
      assert(moduleClass.is(ModuleClass), moduleClass)
      report.debuglog(s"Dumping mirror class for object: $moduleClass")

      val linkedClass  = moduleClass.companionClass
      lazy val conflictingNames: Set[Name] = {
        (linkedClass.info.allMembers.collect { case d if d.name.isTermName => d.name }).toSet
      }
      report.debuglog(s"Potentially conflicting names for forwarders: $conflictingNames")

      for (m0 <- sortedMembersBasedOnFlags(moduleClass.info, required = Method, excluded = ExcludedForwarder)) {
        val m = if (m0.is(Bridge)) m0.nextOverriddenSymbol else m0
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
            // Only hide bridges generated at Erasure, mixin forwarders are also
            // marked as bridge but shouldn't be hidden since they don't have a
            // non-bridge overload.
            m0.is(Bridge) && m0.initial.validFor.firstPhaseId == erasurePhase.next.id
          addForwarder(jclass, moduleClass, m, isSynthetic)
        }
      }
    }

    /** The members of this type that have all of `required` flags but none of `excluded` flags set.
     *  The members are sorted by name and signature to guarantee a stable ordering.
     */
    private def sortedMembersBasedOnFlags(tp: Type, required: Flag, excluded: FlagSet): List[Symbol] = {
      // The output of `memberNames` is a Set, sort it to guarantee a stable ordering.
      val names = tp.memberNames(takeAllFilter).toSeq.sorted
      val buffer = mutable.ListBuffer[Symbol]()
      names.foreach { name =>
        buffer ++= tp.memberBasedOnFlags(name, required, excluded)
          .alternatives.sortBy(_.signature)(Signature.lexicographicOrdering).map(_.symbol)
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
    def getExceptions(excs: List[Annotation]): List[String] = {
      for (case ThrownException(exc) <- excs.distinct)
      yield internalName(TypeErasure.erasure(exc).classSymbol)
    }
  } // end of trait BCForwardersGen

  trait BCClassGen extends BCInnerClassGen {

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
        GenBCodeOps.PrivateStaticFinal,
        "serialVersionUID",
        "J",
        null, // no java-generic-signature
        java.lang.Long.valueOf(id)
      ).visitEnd()
    }
  } // end of trait BCClassGen

  /* functionality for building plain and mirror classes */
  abstract class JCommonBuilder
    extends BCInnerClassGen
    with    BCAnnotGen
    with    BCForwardersGen
    with    BCPickles { }

  /* builder of mirror classes */
  class JMirrorBuilder extends JCommonBuilder {

    private var cunit: CompilationUnit = _
    def getCurrentCUnit(): CompilationUnit = cunit;

    /* Generate a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     *
     *  must-single-thread
     */
    def genMirrorClass(moduleClass: Symbol, cunit: CompilationUnit): asm.tree.ClassNode = {
      assert(moduleClass.is(ModuleClass))
      assert(moduleClass.companionClass == NoSymbol, moduleClass)
      this.cunit = cunit
      val bType      = mirrorClassBTypeFromSymbol(moduleClass)
      val moduleName = internalName(moduleClass) // + "$"
      val mirrorName = bType.internalName

      val mirrorClass = new asm.tree.ClassNode
      mirrorClass.visit(
        classfileVersion,
        bType.info.flags,
        mirrorName,
        null /* no java-generic-signature */,
        ObjectRef.internalName,
        EMPTY_STRING_ARRAY
      )

      if (emitSource) {
        mirrorClass.visitSource("" + cunit.source.file.name,
                                null /* SourceDebugExtension */)
      }

      val ssa = None // getAnnotPickle(mirrorName, if (moduleClass.is(Module)) moduleClass.companionClass else moduleClass.companionModule)
      mirrorClass.visitAttribute(if (ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(mirrorClass, moduleClass.annotations ++ ssa)

      addForwarders(mirrorClass, mirrorName, moduleClass)
      mirrorClass.visitEnd()

      moduleClass.name // this side-effect is necessary, really.

      mirrorClass
    }

  } // end of class JMirrorBuilder

  trait JAndroidBuilder {
    self: BCInnerClassGen =>

    /* From the reference documentation of the Android SDK:
     *  The `Parcelable` interface identifies classes whose instances can be written to and restored from a `Parcel`.
     *  Classes implementing the `Parcelable` interface must also have a static field called `CREATOR`,
     *  which is an object implementing the `Parcelable.Creator` interface.
     */
    val androidFieldName = "CREATOR".toTermName

    lazy val AndroidParcelableInterface : Symbol = NoSymbol // getClassIfDefined("android.os.Parcelable")
    lazy val AndroidCreatorClass        : Symbol = NoSymbol // getClassIfDefined("android.os.Parcelable$Creator")

    /*
     * must-single-thread
     */
    def isAndroidParcelableClass(sym: Symbol) =
      (AndroidParcelableInterface != NoSymbol) &&
      (sym.info.parents.map(_.typeSymbol) contains AndroidParcelableInterface)

    /*
     * must-single-thread
     */
    def legacyAddCreatorCode(clinit: asm.MethodVisitor, cnode: asm.tree.ClassNode, thisName: String): Unit = {
      val androidCreatorType = getClassBType(AndroidCreatorClass)
      val tdesc_creator = androidCreatorType.descriptor

      cnode.visitField(
        GenBCodeOps.PublicStaticFinal,
        "CREATOR",
        tdesc_creator,
        null, // no java-generic-signature
        null  // no initial value
      ).visitEnd()

      val moduleName = (thisName + "$")

      // GETSTATIC `moduleName`.MODULE$ : `moduleName`;
      clinit.visitFieldInsn(
        asm.Opcodes.GETSTATIC,
        moduleName,
        str.MODULE_INSTANCE_FIELD,
        "L" + moduleName + ";"
      )

      // INVOKEVIRTUAL `moduleName`.CREATOR() : android.os.Parcelable$Creator;
      val bt = MethodBType(Nil, androidCreatorType)
      clinit.visitMethodInsn(
        asm.Opcodes.INVOKEVIRTUAL,
        moduleName,
        "CREATOR",
        bt.descriptor,
        false
      )

      // PUTSTATIC `thisName`.CREATOR;
      clinit.visitFieldInsn(
        asm.Opcodes.PUTSTATIC,
        thisName,
        "CREATOR",
        tdesc_creator
      )
    }

  } // end of trait JAndroidBuilder

  /**
   * This method returns the BType for a type reference, for example a parameter type.
   *
   * If the result is a ClassBType for a nested class, it is added to the innerClassBufferASM.
   *
   * If `t` references a class, toTypeKind ensures that the class is not an implementation class.
   * See also comment on getClassBTypeAndRegisterInnerClass, which is invoked for implementation
   * classes.
   */
  private def typeToTypeKind(tp: Type)(ct: BCodeHelpers)(storage: ct.BCInnerClassGen): ct.bTypes.BType = {
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
      assert(sym != defn.ArrayClass || compilingArray, sym)
      primitiveTypeMap.getOrElse(sym, storage.getClassBType(sym)).asInstanceOf[BType]
    }

    /**
      * When compiling Array.scala, the type parameter T is not erased and shows up in method
      * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
      */
    def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
      assert(sym.isType && compilingArray, sym)
      ObjectRef.asInstanceOf[ct.bTypes.ClassBType]
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
        report.debuglog(s"typeKind of annotated type $a")
        typeToTypeKind(t)(ct)(storage)

      /* The cases below should probably never occur. They are kept for now to avoid introducing
        * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
        * test suite don't produce any warning.
        */

      case tp =>
        report.warning(
          s"an unexpected type representation reached the compiler backend while compiling ${ctx.compilationUnit}: $tp. " +
            "If possible, please file a bug on https://github.com/lampepfl/dotty/issues")

        tp match {
          case tp: ThisType if tp.cls == defn.ArrayClass => ObjectRef.asInstanceOf[ct.bTypes.ClassBType] // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
          case tp: ThisType                         => storage.getClassBType(tp.cls)
          // case t: SingletonType                   => primitiveOrClassToBType(t.classSymbol)
          case t: SingletonType                     => typeToTypeKind(t.underlying)(ct)(storage)
          case t: RefinedType                       => typeToTypeKind(t.parent)(ct)(storage) //parents.map(_.toTypeKind(ct)(storage).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b))
        }
    }
  }

  private def getGenericSignatureHelper(sym: Symbol, owner: Symbol, memberTpe: Type)(using Context): Option[String] = {
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
  }

  private def verifySignature(sym: Symbol, sig: String)(using Context): Unit = {
    import scala.tools.asm.util.CheckClassAdapter
    def wrap(body: => Unit): Unit = {
      try body
      catch {
        case ex: Throwable =>
          report.error(i"""|compiler bug: created invalid generic signature for $sym in ${sym.denot.owner.showFullName}
                      |signature: $sig
                      |if this is reproducible, please report bug at https://github.com/lampepfl/dotty/issues
                  """.trim, sym.sourcePos)
          throw  ex
      }
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
    || sym.is(Artifact)
    || sym.isAllOf(LiftedMethod)
    || sym.is(Bridge)
  )

  private def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String = {
    // scala/bug#3452 Static forwarder generation uses the same erased signature as the method if forwards to.
    // By rights, it should use the signature as-seen-from the module class, and add suitable
    // primitive and value-class boxing/unboxing.
    // But for now, just like we did in mixin, we just avoid writing a wrong generic signature
    // (one that doesn't erase to the actual signature). See run/t3452b for a test case.

    val memberTpe = atPhase(erasurePhase) { moduleClass.denot.thisType.memberInfo(sym) }
    val erasedMemberType = ElimErasedValueType.elimEVT(TypeErasure.transformInfo(sym, memberTpe))
    if (erasedMemberType =:= sym.denot.info)
      getGenericSignatureHelper(sym, moduleClass, memberTpe).orNull
    else null
  }

  def abort(msg: String): Nothing = {
    report.error(msg)
    throw new RuntimeException(msg)
  }

  private def compilingArray(using Context) =
    ctx.compilationUnit.source.file.name == "Array.scala"
}

object BCodeHelpers {

  class InvokeStyle(val style: Int) extends AnyVal {
    import InvokeStyle._
    def isVirtual: Boolean = this == Virtual
    def isStatic : Boolean = this == Static
    def isSpecial: Boolean = this == Special
    def isSuper  : Boolean = this == Super

    def hasInstance = this != Static
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

}
