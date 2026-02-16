package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Symbols.{requiredClass as _, *}
import dotty.tools.dotc.transform.Erasure

import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.{StdNames, Symbols}
import BTypes.*
import dotty.tools.dotc.util.ReadOnlyMap
import dotty.tools.dotc.core.Contexts.{Context, atPhase}
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import BCodeAsmCommon.*
import dotty.tools.dotc.core.Flags.{JavaDefined, ModuleClass, PackageClass, Trait}
import dotty.tools.dotc.core.Phases.{Phase, flattenPhase, lambdaLiftPhase}
import DottyBackendInterface.{*, given}
import PostProcessorFrontendAccess.{Lazy, LazyWithoutLock}

import scala.annotation.threadUnsafe
import scala.tools.asm


final class CoreBTypesFromSymbols(val ppa: PostProcessorFrontendAccess)(using val ctx: Context) extends CoreBTypes(ppa) {

  @threadUnsafe private lazy val classBTypeFromInternalNameMap =
    collection.concurrent.TrieMap.empty[String, ClassBType]

  /**
   * Cache for the method classBTypeFromSymbol.
   */
  @threadUnsafe private lazy val convertedClasses = collection.mutable.HashMap.empty[Symbol, ClassBType]

  /**
   * The ClassBType for a class symbol `sym`.
   */
  def classBTypeFromSymbol(classSym: Symbol): ClassBType = {
    assert(classSym != NoSymbol, "Cannot create ClassBType from NoSymbol")
    assert(classSym.isClass, s"Cannot create ClassBType from non-class symbol $classSym")
    assert(
      classSym != defn.NothingClass && classSym != defn.NullClass,
      s"Cannot create ClassBType for special class symbol ${classSym.showFullName}")

    convertedClasses.synchronized:
      convertedClasses.getOrElse(classSym, {
        val internalName = classSym.javaBinaryName
        // We first create and add the ClassBType to the hash map before computing its info. This
        // allows initializing cyclic dependencies, see the comment on variable ClassBType._info.
        val result = classBType(internalName, classSym, true)((ct, cs) => Right(createClassInfo(ct, cs)))
        convertedClasses(classSym) = result
        result
      })
  }

  def mirrorClassBTypeFromSymbol(moduleClassSym: Symbol): ClassBType = {
    assert(moduleClassSym.isTopLevelModuleClass, s"not a top-level module class: $moduleClassSym")
    val internalName = moduleClassSym.javaBinaryName.stripSuffix(StdNames.str.MODULE_SUFFIX)
    classBType(internalName, moduleClassSym, true)((_, mcs) =>
      Right(ClassInfo(
        superClass = Some(ObjectRef),
        interfaces = Nil,
        flags = asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL,
        nestedClasses = LazyWithoutLock(getMemberClasses(mcs).map(classBTypeFromSymbol)),
        nestedInfo = LazyWithoutLock(None)
      ))
    )
  }

  private def createClassInfo(classBType: ClassBType, classSym: Symbol): ClassInfo = {
    val superClassSym: Symbol =  {
      val t = classSym.asClass.superClass
      if (t.exists) t
      else if (classSym.is(ModuleClass)) {
        // workaround #371

        println(s"Warning: mocking up superclass for $classSym")
        defn.ObjectClass
      }
      else t
    }
    assert(
      if (classSym == defn.ObjectClass)
        superClassSym == NoSymbol
      else if (classSym.isInterface)
        superClassSym == defn.ObjectClass
      else
        // A ClassBType for a primitive class (scala.Boolean et al.) is only created when compiling these classes.
        ((superClassSym != NoSymbol) && !superClassSym.isInterface) || primitiveTypeMap.contains(classSym),
      s"Bad superClass for $classSym: $superClassSym"
    )
    val superClass = if (superClassSym == NoSymbol) None
    else Some(classBTypeFromSymbol(superClassSym))

    // List only directly inherited interfaces.
    // This is not only a performance optimization (as the JVM needs to handle fewer inheritance declarations),
    // but also required for correctness in the presence of sealed interfaces (see i23479):
    // if `C` inherits from `non-sealed A` which itself inherits from `sealed B permits A`, then having `C` inherit from `B` directly is illegal.
    val allBaseClasses = classSym.directlyInheritedTraits.iterator.flatMap(_.asClass.baseClasses.drop(1)).toSet
    val interfaces = classSym.directlyInheritedTraits.filter(!allBaseClasses(_)).map(classBTypeFromSymbol)

    val flags = BCodeUtils.javaFlags(classSym)

    /* The InnerClass table of a class C must contain all nested classes of C, even if they are only
     * declared but not otherwise referenced in C (from the bytecode or a method / field signature).
     * We collect them here.
     */
    val nestedClassSymbols = {
      // The lambdalift phase lifts all nested classes to the enclosing class, so if we collect
      // member classes right after lambdalift, we obtain all nested classes, including local and
      // anonymous ones.
      val nestedClasses = getNestedClasses(classSym)

      // If this is a top-level class, and it has a companion object, the member classes of the
      // companion are added as members of the class. For example:
      //   class C { }
      //   object C {
      //     class D
      //     def f = { class E }
      //   }
      // The class D is added as a member of class C. The reason is that the InnerClass attribute
      // for D will containt class "C" and NOT the module class "C$" as the outer class of D.
      // This is done by buildNestedInfo, the reason is Java compatibility, see comment in BTypes.
      // For consistency, the InnerClass entry for D needs to be present in C - to Java it looks
      // like D is a member of C, not C$.
      val linkedClass = classSym.linkedClass
      val companionModuleMembers = {
        if (classSym.linkedClass.isTopLevelModuleClass) getMemberClasses(classSym.linkedClass)
        else Nil
      }

      nestedClasses ++ companionModuleMembers
    }

    /**
     * For nested java classes, the scala compiler creates both a class and a module (and therefore
     * a module class) symbol. For example, in `class A { class B {} }`, the nestedClassSymbols
     * for A contain both the class B and the module class B.
     * Here we get rid of the module class B, making sure that the class B is present.
     */
    val nestedClassSymbolsNoJavaModuleClasses = nestedClassSymbols.filter(s => {
      if (s.is(JavaDefined) && s.is(ModuleClass)) {
        // We could also search in nestedClassSymbols for s.linkedClassOfClass, but sometimes that
        // returns NoSymbol, so it doesn't work.
        val nb = nestedClassSymbols.count(mc => mc.name == s.name && mc.owner == s.owner)
        // this assertion is specific to how ScalaC works. It doesn't apply to dotty, as n dotty there will be B & B$
        // assert(nb == 2, s"Java member module without member class: $s - $nestedClassSymbols")
        false
      } else true
    })

    val memberClasses = nestedClassSymbolsNoJavaModuleClasses.map(classBTypeFromSymbol)

    val nestedInfo = buildNestedInfo(classSym)

    ClassInfo(superClass, interfaces, flags, LazyWithoutLock(memberClasses), LazyWithoutLock(nestedInfo))
  }

  /** For currently compiled classes: All locally defined classes including local classes.
   *  The empty list for classes that are not currently compiled.
   */
  private def getNestedClasses(sym: Symbol): List[Symbol] = definedClasses(sym, flattenPhase)

  /** For currently compiled classes: All classes that are declared as members of this class
   *  (but not inherited ones). The empty list for classes that are not currently compiled.
   */
  private def getMemberClasses(sym: Symbol): List[Symbol] = definedClasses(sym, lambdaLiftPhase)

  private def definedClasses(sym: Symbol, phase: Phase) =
    if (sym.isDefinedInCurrentRun)
      atPhase(phase) {
        toDenot(sym).info.decls.filter(sym => sym.isClass && !sym.isEffectivelyErased)
      }
    else Nil

  private def buildNestedInfo(innerClassSym: Symbol): Option[NestedInfo] = {
    assert(innerClassSym.isClass, s"Cannot build NestedInfo for non-class symbol $innerClassSym")

    val isNested = !innerClassSym.originalOwner.originalLexicallyEnclosingClass.is(PackageClass)
    if (!isNested) None
    else {
      // See comment in BTypes, when is a class marked static in the InnerClass table.
      val isStaticNestedClass = innerClassSym.originalOwner.originalLexicallyEnclosingClass.isOriginallyStaticOwner

      // After lambdalift (which is where we are), the rawowoner field contains the enclosing class.
      val enclosingClassSym = {
        if (innerClassSym.isClass) {
          atPhase(flattenPhase.prev) {
            toDenot(innerClassSym).owner.enclosingClass
          }
        }
        else atPhase(flattenPhase.prev)(innerClassSym.enclosingClass)
      } //todo is handled specially for JavaDefined symbols in scalac

      val enclosingClass: ClassBType = classBTypeFromSymbol(enclosingClassSym)

      val outerName: Option[String] = {
        if (isAnonymousOrLocalClass(innerClassSym)) {
          None
        } else {
          val outerName = innerClassSym.originalOwner.originalLexicallyEnclosingClass.javaBinaryName
          def dropModule(str: String): String =
            if (str.nonEmpty && str.last == '$') str.take(str.length - 1) else str
          // Java compatibility. See the big comment in BTypes that summarizes the InnerClass spec.
          val outerNameModule =
            if (innerClassSym.originalOwner.originalLexicallyEnclosingClass.isTopLevelModuleClass) dropModule(outerName)
            else outerName
          Some(outerNameModule)
        }
      }

      val innerName: Option[String] = {
        if (innerClassSym.isAnonymousClass || innerClassSym.isAnonymousFunction) None
        else {
          val original = innerClassSym.initial
          Some(atPhase(original.validFor.phaseId)(innerClassSym.name).mangledString) // moduleSuffix for module classes
        }
      }

      Some(NestedInfo(enclosingClass, outerName, innerName, isStaticNestedClass))
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
  extension (sym: Symbol)
    private def isOriginallyStaticOwner: Boolean =
      sym.is(PackageClass) || sym.is(ModuleClass) && sym.originalOwner.originalLexicallyEnclosingClass.isOriginallyStaticOwner


  /**
   * Maps primitive types to their corresponding PrimitiveBType. The map is defined lexically above
   * the first use of `classBTypeFromSymbol` because that method looks at the map.
   */
  override def primitiveTypeMap: Map[Symbol, PrimitiveBType] = _primitiveTypeMap.get
  private lazy val _primitiveTypeMap: Lazy[Map[Symbol, PrimitiveBType]] = ppa.perRunLazy:
    Map(
      defn.UnitClass    -> UNIT,
      defn.BooleanClass -> BOOL,
      defn.CharClass    -> CHAR,
      defn.ByteClass    -> BYTE,
      defn.ShortClass   -> SHORT,
      defn.IntClass     -> INT,
      defn.LongClass    -> LONG,
      defn.FloatClass   -> FLOAT,
      defn.DoubleClass  -> DOUBLE
    )

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  override def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = _boxedClassOfPrimitive.get
  private lazy val _boxedClassOfPrimitive: Lazy[Map[PrimitiveBType, ClassBType]] = ppa.perRunLazy(Map(
    UNIT   -> classBTypeFromSymbol(requiredClass[java.lang.Void]),
    BOOL   -> classBTypeFromSymbol(requiredClass[java.lang.Boolean]),
    BYTE   -> classBTypeFromSymbol(requiredClass[java.lang.Byte]),
    SHORT  -> classBTypeFromSymbol(requiredClass[java.lang.Short]),
    CHAR   -> classBTypeFromSymbol(requiredClass[java.lang.Character]),
    INT    -> classBTypeFromSymbol(requiredClass[java.lang.Integer]),
    LONG   -> classBTypeFromSymbol(requiredClass[java.lang.Long]),
    FLOAT  -> classBTypeFromSymbol(requiredClass[java.lang.Float]),
    DOUBLE -> classBTypeFromSymbol(requiredClass[java.lang.Double])
  ))

  lazy val boxedClasses: Set[ClassBType] = boxedClassOfPrimitive.values.toSet

  /**
   * Maps the method symbol for a box method to the boxed type of the result. For example, the
   * method symbol for `Byte.box()` is mapped to the ClassBType `java/lang/Byte`.
   */
  override def boxResultType: Map[Symbol, ClassBType] = _boxResultType.get
  private lazy val _boxResultType: Lazy[Map[Symbol, ClassBType]] = ppa.perRunLazy{
    val boxMethods = defn.ScalaValueClasses().map{x =>
      (x, Erasure.Boxing.boxMethod(x.asClass))
    }.toMap
    for ((valueClassSym, boxMethodSym) <- boxMethods)
      yield boxMethodSym -> boxedClassOfPrimitive(primitiveTypeMap(valueClassSym))
  }

  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()` is mapped to the PrimitiveBType BYTE. */
  override def unboxResultType: Map[Symbol, PrimitiveBType] = _unboxResultType.get
  private lazy val _unboxResultType = ppa.perRunLazy[Map[Symbol, PrimitiveBType]]{
    val unboxMethods: Map[Symbol, Symbol] =
      defn.ScalaValueClasses().map(x => (x, Erasure.Boxing.unboxMethod(x.asClass))).toMap
    for ((valueClassSym, unboxMethodSym) <- unboxMethods)
      yield unboxMethodSym -> primitiveTypeMap(valueClassSym)
  }

  /*
   * srNothingRef and srNullRef exist at run-time only. They are the bytecode-level manifestation (in
   * method signatures only) of what shows up as NothingClass (scala.Nothing) resp. NullClass (scala.Null) in Scala ASTs.
   *
   * Therefore, when srNothingRef or srNullRef are to be emitted, a mapping is needed: the internal
   * names of NothingClass and NullClass can't be emitted as-is.
   * TODO @lry Once there's a 2.11.3 starr, use the commented argument list. The current starr crashes on the type literal `scala.runtime.Nothing$`
   */
  override def srNothingRef: ClassBType = _srNothingRef.get
  private lazy val _srNothingRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass("scala.runtime.Nothing$")))

  override def srNullRef: ClassBType = _srNullRef.get
  private lazy val _srNullRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass("scala.runtime.Null$")))

  override def srBoxedUnitRef: ClassBType = _srBoxedUnitRef.get
  private lazy val _srBoxedUnitRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass("scala.runtime.BoxedUnit")))

  override def ObjectRef: ClassBType = _ObjectRef.get
  private lazy val _ObjectRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(defn.ObjectClass))

  override def StringRef: ClassBType = _StringRef.get
  private lazy val _StringRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(defn.StringClass))

  override def PredefRef: ClassBType = _PredefRef.get
  private lazy val _PredefRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(defn.ScalaPredefModuleClass))

  override def jlClassRef: ClassBType = _jlClassRef.get
  private lazy val _jlClassRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.Class[?]]))

  override def jlThrowableRef: ClassBType = _jlThrowableRef.get
  private lazy val _jlThrowableRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(defn.ThrowableClass))

  override def jlCloneableRef: ClassBType = _jlCloneableRef.get
  private lazy val _jlCloneableRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(defn.JavaCloneableClass))

  override def jiSerializableRef: ClassBType = _jiSerializableRef.get
  private lazy val _jiSerializableRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.io.Serializable]))

  override def jlClassCastExceptionRef: ClassBType = _jlClassCastExceptionRef.get
  private lazy val _jlClassCastExceptionRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.ClassCastException]))

  override def jlIllegalArgExceptionRef: ClassBType = _jlIllegalArgExceptionRef.get
  private lazy val _jlIllegalArgExceptionRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.IllegalArgumentException]))

  override def jliSerializedLambdaRef: ClassBType = _jliSerializedLambdaRef.get
  private lazy val _jliSerializedLambdaRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda]))

  override def srBoxesRuntimeRef: ClassBType = _srBoxesRuntimeRef.get
  private lazy val _srBoxesRuntimeRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime]))

  private def jliCallSiteRef: ClassBType = _jliCallSiteRef.get
  private lazy val _jliCallSiteRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite]))

  private def jliLambdaMetafactoryRef: ClassBType = _jliLambdaMetafactoryRef.get
  private lazy val _jliLambdaMetafactoryRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory]))

  override def jliMethodHandleRef: ClassBType = _jliMethodHandleRef.get
  private lazy val _jliMethodHandleRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(defn.MethodHandleClass))

  private def jliMethodHandlesLookupRef: ClassBType = _jliMethodHandlesLookupRef.get
  private lazy val _jliMethodHandlesLookupRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(defn.MethodHandlesLookupClass))

  private def jliMethodTypeRef: ClassBType = _jliMethodTypeRef.get
  private lazy val _jliMethodTypeRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType]))

  // since JDK 9
  private def jliStringConcatFactoryRef: ClassBType = _jliStringConcatFactoryRef.get
  private lazy val _jliStringConcatFactoryRef: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass("java.lang.invoke.StringConcatFactory")))

  private def srLambdaDeserialize: ClassBType = _srLambdaDeserialize.get
  private lazy val _srLambdaDeserialize: Lazy[ClassBType] = ppa.perRunLazy(classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize]))


  override def jliLambdaMetaFactoryMetafactoryHandle: Handle = _jliLambdaMetaFactoryMetafactoryHandle.get
  private lazy val _jliLambdaMetaFactoryMetafactoryHandle: Lazy[Handle] = ppa.perRunLazy{new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "metafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  override def jliLambdaMetaFactoryAltMetafactoryHandle: Handle = _jliLambdaMetaFactoryAltMetafactoryHandle.get
  private lazy val _jliLambdaMetaFactoryAltMetafactoryHandle: Lazy[Handle] = ppa.perRunLazy{ new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "altMetafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  override def jliLambdaDeserializeBootstrapHandle: Handle = _jliLambdaDeserializeBootstrapHandle.get
  private lazy val _jliLambdaDeserializeBootstrapHandle: Lazy[Handle] = ppa.perRunLazy{ new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserialize.internalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  override def jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle = _jliStringConcatFactoryMakeConcatWithConstantsHandle.get
  private lazy val _jliStringConcatFactoryMakeConcatWithConstantsHandle: Lazy[Handle] = ppa.perRunLazy{ new Handle(
    Opcodes.H_INVOKESTATIC,
    jliStringConcatFactoryRef.internalName,
    "makeConcatWithConstants",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, StringRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  /**
   * Methods in scala.runtime.BoxesRuntime
   * No need to wrap in Lazy to synchronize access, symbols won't change
   */
  lazy val asmBoxTo  : Map[BType, MethodNameAndType] = Map(
    BOOL   -> MethodNameAndType("boxToBoolean",   MethodBType(List(BOOL),   boxedClassOfPrimitive(BOOL))),
    BYTE   -> MethodNameAndType("boxToByte",      MethodBType(List(BYTE),   boxedClassOfPrimitive(BYTE))),
    CHAR   -> MethodNameAndType("boxToCharacter", MethodBType(List(CHAR),   boxedClassOfPrimitive(CHAR))),
    SHORT  -> MethodNameAndType("boxToShort",     MethodBType(List(SHORT),  boxedClassOfPrimitive(SHORT))),
    INT    -> MethodNameAndType("boxToInteger",   MethodBType(List(INT),    boxedClassOfPrimitive(INT))),
    LONG   -> MethodNameAndType("boxToLong",      MethodBType(List(LONG),   boxedClassOfPrimitive(LONG))),
    FLOAT  -> MethodNameAndType("boxToFloat",     MethodBType(List(FLOAT),  boxedClassOfPrimitive(FLOAT))),
    DOUBLE -> MethodNameAndType("boxToDouble",    MethodBType(List(DOUBLE), boxedClassOfPrimitive(DOUBLE)))
  )

  lazy val asmUnboxTo: Map[BType, MethodNameAndType] = Map(
    BOOL   -> MethodNameAndType("unboxToBoolean", MethodBType(List(ObjectRef), BOOL)),
    BYTE   -> MethodNameAndType("unboxToByte",    MethodBType(List(ObjectRef), BYTE)),
    CHAR   -> MethodNameAndType("unboxToChar",    MethodBType(List(ObjectRef), CHAR)),
    SHORT  -> MethodNameAndType("unboxToShort",   MethodBType(List(ObjectRef), SHORT)),
    INT    -> MethodNameAndType("unboxToInt",     MethodBType(List(ObjectRef), INT)),
    LONG   -> MethodNameAndType("unboxToLong",    MethodBType(List(ObjectRef), LONG)),
    FLOAT  -> MethodNameAndType("unboxToFloat",   MethodBType(List(ObjectRef), FLOAT)),
    DOUBLE -> MethodNameAndType("unboxToDouble",  MethodBType(List(ObjectRef), DOUBLE))
  )

  lazy val typeOfArrayOp: Map[Int, BType] = {
    import dotty.tools.backend.ScalaPrimitivesOps.*
    Map(
      (List(ZARRAY_LENGTH, ZARRAY_GET, ZARRAY_SET) map (_ -> BOOL))   ++
        (List(BARRAY_LENGTH, BARRAY_GET, BARRAY_SET) map (_ -> BYTE))   ++
        (List(SARRAY_LENGTH, SARRAY_GET, SARRAY_SET) map (_ -> SHORT))  ++
        (List(CARRAY_LENGTH, CARRAY_GET, CARRAY_SET) map (_ -> CHAR))   ++
        (List(IARRAY_LENGTH, IARRAY_GET, IARRAY_SET) map (_ -> INT))    ++
        (List(LARRAY_LENGTH, LARRAY_GET, LARRAY_SET) map (_ -> LONG))   ++
        (List(FARRAY_LENGTH, FARRAY_GET, FARRAY_SET) map (_ -> FLOAT))  ++
        (List(DARRAY_LENGTH, DARRAY_GET, DARRAY_SET) map (_ -> DOUBLE)) ++
        (List(OARRAY_LENGTH, OARRAY_GET, OARRAY_SET) map (_ -> ObjectRef)) *
    )
  }

  // java/lang/Boolean -> MethodNameAndType(valueOf,(Z)Ljava/lang/Boolean;)
  def javaBoxMethods: Map[InternalName, MethodNameAndType] = _javaBoxMethods.get
  private lazy val _javaBoxMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val unboxed = primitiveTypeMap(primitive)
      val method = MethodNameAndType("valueOf", MethodBType(List(unboxed), boxedClassOfPrimitive(unboxed)))
      (classBTypeFromSymbol(boxed).internalName, method)
    }))
  }

  // java/lang/Boolean -> MethodNameAndType(booleanValue,()Z)
  def javaUnboxMethods: Map[InternalName, MethodNameAndType] = _javaUnboxMethods.get
  private lazy val _javaUnboxMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val name = primitive.name.toString.toLowerCase + "Value"
      (classBTypeFromSymbol(boxed).internalName, MethodNameAndType(name, MethodBType(Nil, primitiveTypeMap(primitive))))
    }))
  }

  private def predefBoxingMethods(isBox: Boolean, getName: (String, String) => String): Map[String, MethodBType] =
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val unboxed = primitiveTypeMap(primitive)
      val boxed = boxedClassOfPrimitive(unboxed)
      val name = getName(primitive.name.toString, defn.boxedClass(primitive).name.toString)
      (name, MethodBType(List(if isBox then unboxed else boxed), if isBox then boxed else unboxed))
    }))

  // boolean2Boolean -> (Z)Ljava/lang/Boolean;
  def predefAutoBoxMethods: Map[String, MethodBType] = _predefAutoBoxMethods.get
  private lazy val _predefAutoBoxMethods: Lazy[Map[String, MethodBType]] = ppa.perRunLazy(predefBoxingMethods(true, (primitive, boxed) => primitive.toLowerCase + "2" + boxed))

  // Boolean2boolean -> (Ljava/lang/Boolean;)Z
  def predefAutoUnboxMethods: Map[String, MethodBType] = _predefAutoUnboxMethods.get
  private lazy val _predefAutoUnboxMethods: Lazy[Map[String, MethodBType]] = ppa.perRunLazy(predefBoxingMethods(false, (primitive, boxed) => boxed + "2" + primitive.toLowerCase))

  // scala/runtime/BooleanRef -> MethodNameAndType(create,(Z)Lscala/runtime/BooleanRef;)
  def srRefCreateMethods: Map[InternalName, MethodNameAndType] = _srRefCreateMethods.get
  private lazy val _srRefCreateMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().union(Set(defn.ObjectClass)).flatMap(primitive => {
      val boxed = if primitive == defn.ObjectClass then primitive else defn.boxedClass(primitive)
      val unboxed = if primitive == defn.ObjectClass then ObjectRef else primitiveTypeMap(primitive)
      val refClass = Symbols.requiredClass("scala.runtime." + primitive.name.toString + "Ref")
      val volatileRefClass = Symbols.requiredClass("scala.runtime.Volatile" + primitive.name.toString + "Ref")
      List(
        (classBTypeFromSymbol(refClass).internalName, MethodNameAndType(nme.create.toString, MethodBType(List(unboxed), classBTypeFromSymbol(refClass)))),
        (classBTypeFromSymbol(volatileRefClass).internalName, MethodNameAndType(nme.create.toString, MethodBType(List(unboxed), classBTypeFromSymbol(volatileRefClass))))
      )
    }))
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(zero,()Lscala/runtime/BooleanRef;)
  def srRefZeroMethods: Map[InternalName, MethodNameAndType] = _srRefZeroMethods.get
  private lazy val _srRefZeroMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().union(Set(defn.ObjectClass)).flatMap(primitive => {
      val boxed = if primitive == defn.ObjectClass then primitive else defn.boxedClass(primitive)
      val refClass = Symbols.requiredClass("scala.runtime." + primitive.name.toString + "Ref")
      val volatileRefClass = Symbols.requiredClass("scala.runtime.Volatile" + primitive.name.toString + "Ref")
      List(
        (classBTypeFromSymbol(refClass).internalName, MethodNameAndType(nme.zero.toString, MethodBType(List(), classBTypeFromSymbol(refClass)))),
        (classBTypeFromSymbol(volatileRefClass).internalName, MethodNameAndType(nme.zero.toString, MethodBType(List(), classBTypeFromSymbol(volatileRefClass))))
      )
    }))
  }

  // java/lang/Boolean -> MethodNameAndType(<init>,(Z)V)
  def primitiveBoxConstructors: Map[InternalName, MethodNameAndType] = _primitiveBoxConstructors.get
  private lazy val _primitiveBoxConstructors: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val unboxed = primitiveTypeMap(primitive)
      (classBTypeFromSymbol(boxed).internalName, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(unboxed), UNIT)))
    }))
  }

  // Z -> MethodNameAndType(boxToBoolean,(Z)Ljava/lang/Boolean;)
  def srBoxesRuntimeBoxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeBoxToMethods.get
  private lazy val _srBoxesRuntimeBoxToMethods: Lazy[Map[BType, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val bType = primitiveTypeMap(primitive)
      val boxed = boxedClassOfPrimitive(bType)
      val name = "boxTo" + defn.boxedClass(primitive).name.toString
      (bType, MethodNameAndType(name, MethodBType(List(bType), boxed)))
    }))
  }

  // Z -> MethodNameAndType(unboxToBoolean,(Ljava/lang/Object;)Z)
  def srBoxesRuntimeUnboxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeUnboxToMethods.get
  private lazy val _srBoxesRuntimeUnboxToMethods: Lazy[Map[BType, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val bType = primitiveTypeMap(primitive)
      val name = "unboxTo" + primitive.name.toString
      (bType, MethodNameAndType(name, MethodBType(List(ObjectRef), bType)))
    }))
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(<init>,(Z)V)
  def srRefConstructors: Map[InternalName, MethodNameAndType] = _srRefConstructors.get
  private lazy val _srRefConstructors: Lazy[Map[InternalName, MethodNameAndType]] =  ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().union(Set(defn.ObjectClass)).flatMap(primitive => {
      val boxed = if primitive == defn.ObjectClass then primitive else defn.boxedClass(primitive)
      val unboxed = if primitive == defn.ObjectClass then ObjectRef else primitiveTypeMap(primitive)
      val refClass = Symbols.requiredClass("scala.runtime." + primitive.name.toString + "Ref")
      val volatileRefClass = Symbols.requiredClass("scala.runtime.Volatile" + primitive.name.toString + "Ref")
      List(
        (classBTypeFromSymbol(refClass).internalName, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(unboxed), UNIT))),
        (classBTypeFromSymbol(volatileRefClass).internalName, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(unboxed), UNIT)))
      )
    }))
  }

  // scala/Tuple3 -> MethodNameAndType(<init>,(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V)
  // scala/Tuple2$mcZC$sp -> MethodNameAndType(<init>,(ZC)V)
  // ... this was easy in scala2, but now we don't specialize them so we have to know each name
  // tuple1 is specialized for D, I, J
  // tuple2 is specialized for C, D, I, J, Z in each parameter
  def tupleClassConstructors: Map[InternalName, MethodNameAndType] = _tupleClassConstructors.get
  private lazy val _tupleClassConstructors: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    val spec1 = List(defn.DoubleClass, defn.IntClass, defn.LongClass)
    val spec2 = List(defn.CharClass, defn.DoubleClass, defn.IntClass, defn.LongClass, defn.BooleanClass)
    Map.from(
      Iterator.concat(
        (1 to 22).map { n =>
          ("scala/Tuple" + n, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List.fill(n)(ObjectRef), UNIT)))
        },
        spec1.map { sp1 =>
          val prim = primitiveTypeMap(sp1)
          ("scala/Tuple1$mc" + prim.descriptor + "$sp", MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(), UNIT)))
        },
        for sp2a <- spec2; sp2b <- spec2 yield {
          val primA = primitiveTypeMap(sp2a)
          val primB = primitiveTypeMap(sp2b)
          ("scala/Tuple2$mc" + primA.descriptor + primB.descriptor + "$sp", MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(primA, primB), UNIT)))
        }
      )
    )
  }
}
