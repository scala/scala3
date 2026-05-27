package dotty.tools
package backend
package jvm

import java.util.concurrent.ConcurrentHashMap
import BTypes.InternalName
import dotty.tools.backend.jvm.BCodeUtils.isAnonymousOrLocalClass
import dotty.tools.backend.jvm.SymbolUtils.symExtensions
import dotty.tools.dotc.core.Symbols.{ClassSymbol, NoSymbol, Symbol, defn}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.core.Flags.{Final, JavaDefined, Method, ModuleClass, ModuleVal, PackageClass, Trait}
import dotty.tools.dotc.core.Phases.{Phase, flattenPhase, lambdaLiftPhase, picklerPhase}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.{StdNames, Types}
import dotty.tools.dotc.core.Types.{JavaArrayType, Type, TypeRef, abstractTermNameFilter}

import scala.annotation.tailrec
import scala.tools.asm
import scala.tools.asm.tree.ClassNode

final class BTypeLoader(primitives: ScalaPrimitives, inlineInfoLoader: () => Option[InlineInfoLoader]) {
  // Concurrent map because stack map frames are computed when in the class writer, which
  // might run on multiple classes concurrently.
  private val classBTypeCache = new ConcurrentHashMap[InternalName, ClassBType]

  /** Maps special symbols, including primitive types, to their corresponding BType. */
  // It's OK to cache this because all Contexts that go through here share their defns.
  // No locking, it's OK if this map gets initialized twice (though a little inefficient).
  private var specialBTypes: Map[Symbol, BType] | Null = null


  /** See doc of ClassBType.apply. This is where to use that method from. */
  def classBType[T](internalName: InternalName)(init: ClassBType => Either[T, ClassInfo]): Either[T, ClassBType] =
    ClassBType(internalName, classBTypeCache)(init)

  /** See doc of ClassBType.apply. This is where to use that method from. Version that cannot fail. */
  def classBType(internalName: InternalName)(init: ClassBType => ClassInfo): ClassBType =
    ClassBType(internalName, classBTypeCache)(ct => Right(init(ct))).fold(_ => assert(false), identity)

  /** Obtain a previously constructed ClassBType for a given internal name, or None if no such ClassBType was constructed. */
  def previouslyConstructedClassBType(internalName: InternalName): Option[ClassBType] =
    Option(classBTypeCache.get(internalName))

  def bTypeFromSymbol(sym: Symbol)(using Context): BType = {
    if specialBTypes eq null then
      specialBTypes = Map(
        defn.UnitClass -> UNIT,
        defn.BooleanClass -> BOOL,
        defn.CharClass -> CHAR,
        defn.ByteClass -> BYTE,
        defn.ShortClass -> SHORT,
        defn.IntClass -> INT,
        defn.LongClass -> LONG,
        defn.FloatClass -> FLOAT,
        defn.DoubleClass -> DOUBLE,
        defn.NothingClass -> classBTypeFromSymbol(defn.RuntimeNothingClass),
        defn.NullClass -> classBTypeFromSymbol(defn.RuntimeNullClass)
      )
    specialBTypes.nn.getOrElse(sym, classBTypeFromSymbol(sym))
  }

  /**
   * The ClassBType for a class symbol `sym`.
   */
  def classBTypeFromSymbol(classSym0: Symbol)(using Context): ClassBType = {
    // For each java class, the scala compiler creates a class and a module (thus a module class).
    // If the symbol is a java module class, we use the java class instead. This ensures that the
    // ClassBType is created from the main class (instead of the module class).
    // The two symbols have the same name, so the resulting internalName is the same.
    val classSym = if classSym0.isAllOf(JavaDefined | ModuleClass, butNot = PackageClass)
                   then classSym0.linkedClass
                   else classSym0

    assert(classSym.isClass, s"Cannot create ClassBType from non-class symbol $classSym") // also covers the NoSymbol case
    assert(
      classSym != defn.NothingClass && classSym != defn.NullClass,
      s"Cannot create ClassBType for special class symbol ${classSym.showFullName}")
    assert(classSym != defn.ArrayClass || compilingArray, classSym)
    assert(!classSym.isPrimitiveValueClass || compilingPrimitive, s"Found $classSym while compiling ${ctx.compilationUnit.source.file.name}")

    classBType(classSym.javaBinaryName)(ct => createClassInfo(ct, classSym.asClass))
  }

  def mirrorClassBTypeFromSymbol(moduleClassSym: Symbol)(using Context): ClassBType = {
    assert(moduleClassSym.isTopLevelModuleClass, s"not a top-level module class: $moduleClassSym")
    val internalName = moduleClassSym.javaBinaryName.stripSuffix(StdNames.str.MODULE_SUFFIX)
    classBType(internalName)(_ =>
      ClassInfo(
        superClass = Some(classBTypeFromSymbol(defn.ObjectClass)),
        interfaces = Nil,
        flags = asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL,
        nestedClasses = getMemberClasses(moduleClassSym).map(classBTypeFromSymbol),
        nestedInfo = None,
        inlineInfo = InlineInfo.empty
      )
    )
  }

  /*
   * must-single-thread
   */
  def methodBTypeFromSymbol(msym: Symbol)(using Context): MethodBType = {
    assert(msym.is(Method), s"not a method-symbol: $msym")
    val resT: BType =
      if (msym.isClassConstructor || msym.isConstructor) UNIT
      else bTypeFromType(msym.info.resultType)
    MethodBType(msym.info.firstParamTypes.map(bTypeFromType), resT)
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
  def bTypeFromType(tp: Type)(using Context): BType = {
    tp.widenDealias match
      case JavaArrayType(el) => ArrayBType(bTypeFromType(el)) // Array type such as Array[Int] (kept by erasure)
      case t: TypeRef => bTypeFromSymbol(t.symbol) // Common reference to a type such as scala.Int or java.lang.String
      case Types.ClassInfo(_, sym, _, _, _) => bTypeFromSymbol(sym) // We get here, for example, for genCallMethod, which invokes bTypeFromType(method.owner.info)
      case tp =>
        throw new AssertionError(s"an unexpected type representation reached the compiler backend while compiling ${ctx.compilationUnit}: $tp.")
  }

  /**
   * Visit the class node and collect all referenced nested classes.
   */
  def collectNestedClasses(classNode: ClassNode): (Iterable[ClassBType], Iterable[ClassBType]) = {
    val c = new NestedClassesCollector[ClassBType](nestedOnly = true) {
      def declaredNestedClasses(internalName: InternalName): List[ClassBType] =
        previouslyConstructedClassBType(internalName).get.info.nestedClasses

      def getClassIfNested(internalName: InternalName): Option[ClassBType] = {
        val c = previouslyConstructedClassBType(internalName).get
        Option.when(c.isNestedClass)(c)
      }

      def raiseError(msg: String, sig: String, e: Option[Throwable]): Unit = {
        // don't crash on invalid generic signatures
      }
    }
    c.visit(classNode)
    (c.declaredInnerClasses, c.referredInnerClasses)
  }

  private def createClassInfo(classBType: ClassBType, classSym: Symbol)(using Context): ClassInfo = {
    val superClassSym: Symbol = {
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
      else if (classSym.is(Trait))
        superClassSym == defn.ObjectClass
      else
        // A ClassBType for a primitive class (scala.Boolean et al.) is only created when compiling these classes.
        ((superClassSym != NoSymbol) && !superClassSym.is(Trait)) || classSym.isPrimitiveValueClass,
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
      // for D will contain class "C" and NOT the module class "C$" as the outer class of D.
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
     *
     * (In Scala 2, we had an assertion that there must be exactly 2 nested class symbols with the same name and owner,
     * but in Dotty there will be B & B$)
     */
    val nestedClassSymbolsNoJavaModuleClasses = nestedClassSymbols.filter(s => !(s.is(JavaDefined) && s.is(ModuleClass)))

    val memberClasses = nestedClassSymbolsNoJavaModuleClasses.map(classBTypeFromSymbol)

    val nestedInfo = buildNestedInfo(classSym)

    val inlineInfo = inlineInfoLoader() match {
      case Some(loader) => buildInlineInfo(loader, classSym.asClass, classBType.internalName)
      case None => InlineInfo.empty
    }

    ClassInfo(superClass, interfaces, flags, memberClasses, nestedInfo, inlineInfo)
  }

  /** For currently compiled classes: All locally defined classes including local classes.
   * The empty list for classes that are not currently compiled.
   */
  private def getNestedClasses(sym: Symbol)(using Context): List[Symbol] = definedClasses(sym, flattenPhase)

  /** For currently compiled classes: All classes that are declared as members of this class
   * (but not inherited ones). The empty list for classes that are not currently compiled.
   */
  private def getMemberClasses(sym: Symbol)(using Context): List[Symbol] = definedClasses(sym, lambdaLiftPhase)

  private def definedClasses(sym: Symbol, phase: Phase)(using Context) =
    if (sym.isDefinedInCurrentRun)
      atPhase(phase) {
        sym.info.decls.filter(sym => sym.isClass && !sym.isEffectivelyErased)
      }
    else Nil

  private def buildNestedInfo(innerClassSym: Symbol)(using Context): Option[NestedInfo] = {
    assert(innerClassSym.isClass, s"Cannot build NestedInfo for non-class symbol $innerClassSym")

    val isNested = !innerClassSym.originalOwner.originalLexicallyEnclosingClass.is(PackageClass)
    if (!isNested) None
    else {
      // See comment in BTypes, when is a class marked static in the InnerClass table.
      val isStaticNestedClass = innerClassSym.is(ModuleClass) || isOriginallyStaticOwner(innerClassSym.originalOwner.originalLexicallyEnclosingClass)

      // After lambdalift (which is where we are), the raw owner field contains the enclosing class.
      val enclosingClassSym = {
        if (innerClassSym.isClass) {
          atPhase(flattenPhase.prev) {
            innerClassSym.owner.enclosingClass
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
          Some(atPhase(original.validFor.lastPhaseId)(innerClassSym.targetName).mangledString) // moduleSuffix for module classes
        }
      }

      Some(NestedInfo(enclosingClass, outerName, innerName, isStaticNestedClass))
    }
  }

  /*
   * Note that the InlineInfo is only built from the symbolic information for classes that are being
   * compiled. For all other classes we delegate to inlineInfoFromClassfile. The reason is that
   * mixed-in methods are only added to class symbols being compiled, but not to other classes
   * extending traits. Creating the InlineInfo from the symbol would prevent these mixins from being
   * inlined.
   *
   * So for classes being compiled, the InlineInfo is created here and stored in the ScalaInlineInfo
   * classfile attribute.
   */
  private def buildInlineInfo(inlineInfoLoader: InlineInfoLoader, classSym: ClassSymbol, internalName: InternalName)(using Context): InlineInfo = {
    // phase travel required (or at least it was in Scala 2). for nested classes, it checks if the
    // enclosingTopLevelClass is being compiled. after flatten, all classes are considered top-level,
    // so it would return `false`.
    if atPhase(picklerPhase.next) {
      classSym.isDefinedInCurrentRun
    } then buildInlineInfoFromClassSymbol(classSym) // // InlineInfo required for classes being compiled, we have to create the classfile attribute
    // For classes not being compiled, the InlineInfo is read from the classfile attribute. This
    // fixes an issue with mixed-in methods: the mixin phase enters mixin methods only to class
    // symbols being compiled. For non-compiled classes, we could not build MethodInlineInfos
    // for those mixin members, which prevents inlining.
    else inlineInfoLoader.loadInlineInfoFor(internalName)
  }

  /**
   * Build the [[InlineInfo]] for a class symbol.
   */
  private def buildInlineInfoFromClassSymbol(classSym: ClassSymbol)(using Context): InlineInfo = {
    // We only want an approximation of SAMs for inlining heuristics, no need to check FunctionalInterface annotations or such
    val abstractMembers = classSym.memberNames(abstractTermNameFilter).iterator.map(classSym.classInfo.member).map(_.symbol).filter(_.is(Method)).toList
    val sam = abstractMembers match
      case List(single) =>
        val btype = methodBTypeFromSymbol(single)
        Some(single.javaSimpleName + btype.descriptor)
      case _ => None

    def keepMember(sym: Symbol) = sym.is(Method) && !primitives.isPrimitive(sym)

    val classMethods = classSym.info.decls.iterator.filter(keepMember)
    val methods = if classSym.is(JavaDefined) then
      // Phase travel important for nested classes (scala-dev#402). When a java class symbol A$B
      // is compiled from source, this ensures that `companionModule` doesn't return the `A$B`
      // symbol created for the `A$B.class` file on the classpath, which might be different.
      val companion = atPhase(picklerPhase.next) {
        classSym.companionModule
      }
      val staticMethods = companion.info.decls.iterator.filter(m => !m.isConstructor && keepMember(m))
      staticMethods ++ classMethods
    else
      val staticForwarders = if classSym.is(Trait) then
        // !!! This logic duplicates PlainSkelBuilder::makeStaticForwarder, copy changes there !!!
        classSym.info.decls.filter(s => s.isTerm && !s.isPrivate && !s.isStaticMember && s.name != nme.TRAIT_CONSTRUCTOR).map(s => {
          SymbolUtils.makeStatifiedDefSymbol(s.asTerm, SymbolUtils.traitSuperAccessorName(s).toTermName)
        })
      else Nil
      classMethods ++ staticForwarders

    // Primitive methods cannot be inlined, so there's no point in building a MethodInlineInfo. Also, some
    // primitive methods (e.g., `isInstanceOf`) have non-erased types, which confuses [[typeToBType]].
    val methodInlineInfos = new collection.mutable.TreeMap[(String, String), MethodInlineInfo]()
    methods.foreach {
      methodSym =>
        val name = methodSym.javaSimpleName // same as in genDefDef
        val signature = (name, methodBTypeFromSymbol(methodSym).descriptor)

        // In a trait, accesses to "modules" like enums are translated by the frontend as final methods,
        // even though they are logically not final since classes implementing the trait will also have that method,
        // so we must explicitly consider them to be non-final.
        // TODO: This feels like something fundamentally weird in trees that should not exist.
        val info = MethodInlineInfo(
          effectivelyFinal = methodSym.isEffectivelyFinal && !methodSym.is(ModuleVal),
          annotatedInline = methodSym.hasAnnotation(defn.InlineAnnot),
          annotatedNoInline = methodSym.hasAnnotation(defn.NoInlineAnnot))

        methodInlineInfos(signature) = info
    }

    // if we have a symbol, we're compiling the class, so we assume it's accessible
    InlineInfo(classSym.is(Final), sam, methodInlineInfos, None, isAccessible = true)
  }

  /**
   * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
   *
   * The problem is that we are interested in a source-level property. Various phases changed the
   * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
   * Therefore, `sym.isStatic` is not what we want. For example, in
   * object T { def f { object U } }
   * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
   */
  @tailrec
  private def isOriginallyStaticOwner(sym: Symbol)(using Context): Boolean =
    sym.is(PackageClass) || sym.is(ModuleClass) && isOriginallyStaticOwner(sym.originalOwner.originalLexicallyEnclosingClass)
  
  private def compilingArray(using Context) =
    ctx.compilationUnit.source.file.name == "Array.scala"

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
  private def compilingPrimitive(using Context) =
    primitiveCompilationUnits(ctx.compilationUnit.source.file.name)
}