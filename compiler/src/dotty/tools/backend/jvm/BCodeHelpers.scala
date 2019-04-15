package dotty.tools
package backend
package jvm

import scala.tools.asm
import scala.collection.mutable
import dotty.tools.io.AbstractFile

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
  import coreBTypes._
  import int._

  val bCodeAsmCommon: BCodeAsmCommon[int.type] = new BCodeAsmCommon(int)
  import bCodeAsmCommon._

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
      csym.outputDirectory
    } catch {
      case ex: Throwable =>
        int.error(csym.pos, s"Couldn't create file for class $cName\n${ex.getMessage}")
        null
    }
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
  def initBytecodeWriter(entryPoints: List[Symbol]): BytecodeWriter = {
    getSingleOutput match {
      case Some(f) if f hasExtension "jar" =>
        // If no main class was specified, see if there's only one
        // entry point among the classes going into the jar.
        if (mainClass.isEmpty) {
          entryPoints map (_.fullName('.')) match {
            case Nil =>
              log("No Main-Class designated or discovered.")
            case name :: Nil =>
              log(s"Unique entry point: setting Main-Class to $name")
              setMainClass(name)
            case names =>
              log(s"No Main-Class due to multiple entry points:\n  ${names.mkString("\n  ")}")
          }
        }
        else log(s"Main-Class was specified: ${mainClass.get}")

        new DirectToJarfileWriter(f.file)

      case _ => factoryNonJarBytecodeWriter()
    }
  }

  /*
   * Populates the InnerClasses JVM attribute with `refedInnerClasses`.
   * In addition to inner classes mentioned somewhere in `jclass` (where `jclass` is a class file being emitted)
   * `refedInnerClasses` should contain those inner classes defined as direct member classes of `jclass`
   * but otherwise not mentioned in `jclass`.
   *
   * `refedInnerClasses` may contain duplicates,
   * need not contain the enclosing inner classes of each inner class it lists (those are looked up for consistency).
   *
   * This method serializes in the InnerClasses JVM attribute in an appropriate order,
   * not necessarily that given by `refedInnerClasses`.
   *
   * can-multi-thread
   */
  final def addInnerClassesASM(jclass: asm.ClassVisitor, refedInnerClasses: List[ClassBType]): Unit = {
    val allNestedClasses = refedInnerClasses.flatMap(_.enclosingNestedClassesChain).distinct

    // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    for (nestedClass <- allNestedClasses.sortBy(_.internalName.toString)) {
      // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
      val Some(e) = nestedClass.innerClassAttributeEntry
      jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
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
    def createJAttribute(name: String, b: Array[Byte], offset: Int, len: Int): asm.Attribute = {
      val dest = new Array[Byte](len)
      System.arraycopy(b, offset, dest, 0, len)
      new asm.CustomAttr(name, dest)
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

    def debugLevel = int.debuglevel

    final val emitSource = debugLevel >= 1
    final val emitLines  = debugLevel >= 2
    final val emitVars   = debugLevel >= 3

    /*
     *  Contains class-symbols that:
     *    (a) are known to denote inner classes
     *    (b) are mentioned somewhere in the class being generated.
     *
     *  In other words, the lifetime of `innerClassBufferASM` is associated to "the class being generated".
     */
    final val innerClassBufferASM = mutable.Set.empty[ClassBType]

    /**
     * The class internal name for a given class symbol. If the symbol describes a nested class, the
     * ClassBType is added to the innerClassBufferASM.
     */
    final def internalName(sym: Symbol): String = {
      // For each java class, the scala compiler creates a class and a module (thus a module class).
      // If the `sym` is a java module class, we use the java class instead. This ensures that we
      // register the class (instead of the module class) in innerClassBufferASM.
      // The two symbols have the same name, so the resulting internalName is the same.
      val classSym = if (sym.isJavaDefined && sym.isModuleClass) sym.linkedClassOfClass else sym
      getClassBTypeAndRegisterInnerClass(classSym).internalName
    }

    private def assertClassNotArray(sym: Symbol): Unit = {
      assert(sym.isClass, sym)
      assert(sym != ArrayClass || isCompilingArray, sym)
    }

    private def assertClassNotArrayNotPrimitive(sym: Symbol): Unit = {
      assertClassNotArray(sym)
      assert(!primitiveTypeMap.contains(sym) || isCompilingPrimitive, sym)
    }

    /**
     * The ClassBType for a class symbol. If the class is nested, the ClassBType is added to the
     * innerClassBufferASM.
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
    final def getClassBTypeAndRegisterInnerClass(sym: Symbol): ClassBType = {
      assertClassNotArrayNotPrimitive(sym)

      if (sym == NothingClass) RT_NOTHING
      else if (sym == NullClass) RT_NULL
      else {
       val r = classBTypeFromSymbol(sym)
        if (r.isNestedClass) innerClassBufferASM += r
        r
      }
    }

    /*
     * must-single-thread
     */
    final def asmMethodType(msym: Symbol): MethodBType = {
      assert(msym.isMethod, s"not a method-symbol: $msym")
      val resT: BType =
        if (msym.isClassConstructor || msym.isConstructor) UNIT
        else toTypeKind(msym.tpe.resultType)
      MethodBType(msym.tpe.paramTypes map toTypeKind, resT)
    }

    /**
     * The jvm descriptor of a type. If `t` references a nested class, its ClassBType is added to
     * the innerClassBufferASM.
     */
    final def typeDescriptor(t: Type): String = { toTypeKind(t).descriptor   }

    /**
     * The jvm descriptor for a symbol. If `sym` represents a nested class, its ClassBType is added
     * to the innerClassBufferASM.
     */
    final def symDescriptor(sym: Symbol): String = { getClassBTypeAndRegisterInnerClass(sym).descriptor }

    final def toTypeKind(tp: Type): BType = tp.toTypeKind(BCodeHelpers.this)(this)

  } // end of trait BCInnerClassGen

  trait BCAnnotGen extends BCInnerClassGen {

    /*
     * must-single-thread
     */
    def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation]) =
      int.emitAnnotations(cw, annotations, BCodeHelpers.this)(this)

    /*
     * must-single-thread
     */
    def emitAnnotations(mw: asm.MethodVisitor, annotations: List[Annotation]) =
      int.emitAnnotations(mw, annotations, BCodeHelpers.this)(this)

    /*
     * must-single-thread
     */
    def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation]) =
      int.emitAnnotations(fw, annotations, BCodeHelpers.this)(this)

    /*
     * must-single-thread
     */
    def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[Annotation]]) =
      int.emitParamAnnotations(jmethod, pannotss, BCodeHelpers.this)(this)

  } // end of trait BCAnnotGen

  trait BCJGenSigGen {

    def getCurrentCUnit(): CompilationUnit

    /* @return
     *   - `null` if no Java signature is to be added (`null` is what ASM expects in these cases).
     *   - otherwise the signature in question
     *
     * must-single-thread
     */
    def getGenericSignature(sym: Symbol, owner: Symbol): String = int.getGenericSignature(sym, owner)

  } // end of trait BCJGenSigGen

  trait BCForwardersGen extends BCAnnotGen with BCJGenSigGen {

    /* Add a forwarder for method m. Used only from addForwarders().
     *
     * must-single-thread
     */
    private def addForwarder(jclass: asm.ClassVisitor, module: Symbol, m: Symbol): Unit = {
      val moduleName     = internalName(module)
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes: List[BType] = methodInfo.paramTypes map toTypeKind
      // val paramNames     = 0 until paramJavaTypes.length map ("x_" + _)

      /* Forwarders must not be marked final,
       *  as the JVM will not allow redefinition of a final static method,
       *  and we don't know what classes might be subclassing the companion class.  See SI-4827.
       */
      // TODO: evaluate the other flags we might be dropping on the floor here.
      // TODO: ACC_SYNTHETIC ?
      val flags = GenBCodeOps.PublicStatic | (
        if (m.isVarargsMethod) asm.Opcodes.ACC_VARARGS else 0
      )

      // TODO needed? for(ann <- m.annotations) { ann.symbol.initialize }
      val jgensig = getStaticForwarderGenericSignature(m, module)
      val (throws, others) = m.annotations partition (_.symbol == ThrowsClass)
      val thrownExceptions: List[String] = getExceptions(throws)

      val jReturnType = toTypeKind(methodInfo.resultType)
      val mdesc = MethodBType(paramJavaTypes, jReturnType).descriptor
      val mirrorMethodName = m.javaSimpleName.toString
      val mirrorMethod: asm.MethodVisitor = jclass.visitMethod(
        flags,
        mirrorMethodName,
        mdesc,
        jgensig,
        mkArrayS(thrownExceptions)
      )

      emitAnnotations(mirrorMethod, others)
      emitParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))

      mirrorMethod.visitCode()

      mirrorMethod.visitFieldInsn(asm.Opcodes.GETSTATIC, moduleName, MODULE_INSTANCE_FIELD, symDescriptor(module))

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
      assert(moduleClass.isModuleClass, moduleClass)
      debuglog(s"Dumping mirror class for object: $moduleClass")

      val linkedClass  = moduleClass.companionClass
      lazy val conflictingNames: Set[Name] = {
        (linkedClass.info.members collect { case sym if sym.name.isTermName => sym.name }).toSet
      }
      debuglog(s"Potentially conflicting names for forwarders: $conflictingNames")

      for (m0 <- moduleClass.info.membersBasedOnFlags(ExcludedForwarderFlags, Flag_METHOD)) {
        val m = if (m0.isBridge) m0.nextOverriddenSymbol else m0
        if (m == NoSymbol)
          log(s"$m0 is a bridge method that overrides nothing, something went wrong in a previous phase.")
        else if (m.isType || m.isDeferred || (m.owner eq ObjectClass) || m.isConstructor || m.isExpanded)
          debuglog(s"No forwarder for '$m' from $jclassName to '$moduleClass'")
        else if (conflictingNames(m.name))
          log(s"No forwarder for $m due to conflict with ${linkedClass.info.member(m.name)}")
        else if (m.hasAccessBoundary)
          log(s"No forwarder for non-public member $m")
        else {
          log(s"Adding static forwarder for '$m' from $jclassName to '$moduleClass'")
          addForwarder(jclass, moduleClass, m)
        }
      }
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
      for (ThrownException(exc) <- excs.distinct)
      yield internalName(exc)
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
        GenBCodeOps.PublicStaticFinal,
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
      assert(moduleClass.isModuleClass)
      assert(moduleClass.companionClass == NoSymbol, moduleClass)
      innerClassBufferASM.clear()
      this.cunit = cunit
      val moduleName = internalName(moduleClass) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)

      val flags = (asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL)
      val mirrorClass = new asm.tree.ClassNode
      mirrorClass.visit(
        classfileVersion,
        flags,
        mirrorName,
        null /* no java-generic-signature */,
        ObjectReference.internalName,
        EMPTY_STRING_ARRAY
      )

      if (emitSource) {
        mirrorClass.visitSource("" + sourceFileFor(cunit),
                                null /* SourceDebugExtension */)
      }

      val ssa = getAnnotPickle(mirrorName, moduleClass.companionSymbol)
      mirrorClass.visitAttribute(if (ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(mirrorClass, moduleClass.annotations ++ ssa)

      addForwarders(mirrorClass, mirrorName, moduleClass)

      innerClassBufferASM ++= classBTypeFromSymbol(moduleClass).info.memberClasses
      addInnerClassesASM(mirrorClass, innerClassBufferASM.toList)

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
    val androidFieldName = newTermName("CREATOR")

    lazy val AndroidParcelableInterface : Symbol = getClassIfDefined("android.os.Parcelable")
    lazy val AndroidCreatorClass        : Symbol = getClassIfDefined("android.os.Parcelable$Creator")

    /*
     * must-single-thread
     */
    def isAndroidParcelableClass(sym: Symbol) =
      (AndroidParcelableInterface != NoSymbol) &&
      (sym.parentSymbols contains AndroidParcelableInterface)

    /*
     * must-single-thread
     */
    def legacyAddCreatorCode(clinit: asm.MethodVisitor, cnode: asm.tree.ClassNode, thisName: String): Unit = {
      // this tracks the inner class in innerClassBufferASM, if needed.
      val androidCreatorType = getClassBTypeAndRegisterInnerClass(AndroidCreatorClass)
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
        MODULE_INSTANCE_FIELD,
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
}
