package dotty.tools.backend.jvm.opt

import scala.jdk.CollectionConverters.*
import dotty.tools.backend.jvm.BackendReporting.{ClassNotFoundWhenBuildingInlineInfoFromSymbol, NoInlineInfoAttribute}
import dotty.tools.backend.jvm.*

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.{Final, JavaDefined, Method, ModuleVal, Sealed}
import dotty.tools.dotc.core.Symbols.{requiredClass as _, *}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Types.abstractTermNameFilter
import dotty.tools.dotc.core.Phases.picklerPhase
import DottyBackendInterface.given

import scala.annotation.tailrec
import scala.collection.{SortedMap, mutable}
import scala.collection.immutable.ArraySeq
import scala.tools.asm.Opcodes.{ACC_ABSTRACT, ACC_PRIVATE, ACC_PROTECTED, ACC_PUBLIC, ACC_STATIC}
import scala.tools.asm.tree.{ClassNode, ModuleNode}

/**
 * Metadata about a ClassBType, used by the inliner.
 *
 * More information may be added in the future to enable more elaborate inline heuristics.
 * Note that this class should contain information that can only be obtained from the ClassSymbol.
 * Information that can be computed from the ClassNode should be added to the call graph instead.
 *
 * @param isEffectivelyFinal True if the class cannot have subclasses: final classes, module
 *                           classes.
 *
 * @param sam                If this class is a SAM type, the SAM's "\$name\$descriptor".
 * @param methodInfos        The [[MethodInlineInfo]]s for the methods declared in this class.
 *                           The map is indexed by the string s"\$name\$descriptor" (to
 *                           disambiguate overloads).
 *
 * @param warning            Contains a warning message if an error occurred when building this
 *                           InlineInfo, for example if some classfile could not be found on
 *                           the classpath. This warning can be reported later by the inliner.
 *
 * @param isAccessible       Whether this class's internals can be inlined into callsites, i.e.,
 *                           it is exported from a public module.
 */
final case class InlineInfo(isEffectivelyFinal: Boolean,
                            sam: Option[String],
                            methodInfos: collection.SortedMap[(String, String), MethodInlineInfo],
                            warning: Option[BackendReporting.ClassInlineInfoWarning],
                            isAccessible: Boolean) {
  lazy val methodInfosSorted: IndexedSeq[((String, String), MethodInlineInfo)] = {
    val result = new Array[((String, String), MethodInlineInfo)](methodInfos.size)
    var i = 0
    methodInfos.foreachEntry { (ownerAndName, info) =>
      result(i) = (ownerAndName, info)
      i += 1
    }
    scala.util.Sorting.quickSort(result)(using Ordering.by(_._1))
    ArraySeq.unsafeWrapArray(result)
  }
}

/**
 * Metadata about a method, used by the inliner.
 *
 * @param effectivelyFinal  True if the method cannot be overridden (in Scala)
 * @param annotatedInline   True if the method is annotated `@inline`
 * @param annotatedNoInline True if the method is annotated `@noinline`
 */
final case class MethodInlineInfo(effectivelyFinal: Boolean = false,
                                  annotatedInline: Boolean = false,
                                  annotatedNoInline: Boolean = false)

final class InlineInfoLoader(bCodeRepository: BCodeRepository, primitives: DottyPrimitives, ts: CoreBTypes)(using Context) {
  private val infos: mutable.Map[ClassInfo, InlineInfo] = mutable.Map.empty

  /**
   * Build the InlineInfo for a ClassInfo.
   *
   * Note that the InlineInfo is only built from the symbolic information for classes that are being
   * compiled. For all other classes we delegate to inlineInfoFromClassfile. The reason is that
   * mixed-in methods are only added to class symbols being compiled, but not to other classes
   * extending traits. Creating the InlineInfo from the symbol would prevent these mixins from being
   * inlined.
   *
   * So for classes being compiled, the InlineInfo is created here and stored in the ScalaInlineInfo
   * classfile attribute.
   */
  def load(bt: ClassInfo): InlineInfo =
    infos.getOrElseUpdate(bt, {
      bt.source match {
        case ClassInfoSource.Symbol(classSym, internalName) =>
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
          else bCodeRepository.classNode(internalName) match {
            case Right(classNode, moduleNode) =>
              inlineInfoFromClassfile(classNode, moduleNode)
            case Left(missingClass) =>
              InlineInfo.empty.copy(warning = Some(ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass)))
          }
        case ClassInfoSource.Classfile(classNode, moduleNode) => inlineInfoFromClassfile(classNode, moduleNode)
        case ClassInfoSource.Missing => InlineInfo.empty
      }
    })

  /**
   * Build the [[InlineInfo]] for a class symbol.
   */
  private def buildInlineInfoFromClassSymbol(classSym: ClassSymbol): InlineInfo = {
    // We only want an approximation of SAMs for inlining heuristics, no need to check FunctionalInterface annotations or such
    val abstractMembers = classSym.memberNames(abstractTermNameFilter).iterator.map(classSym.classInfo.member).map(_.symbol).filter(_.is(Method)).toList
    val sam = abstractMembers match
      case List(single) =>
        val btype = ts.asmMethodType(single)
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
      val staticForwarders = if classSym.isInterface then
        classSym.info.decls.filter(s => !s.isPrivate && !s.isStaticMember && s.name != nme.TRAIT_CONSTRUCTOR).map(s => {
          BackendUtils.makeStatifiedDefSymbol(s.asTerm, BackendUtils.traitSuperAccessorName(s).toTermName)
        })
      else Nil
      classMethods ++ staticForwarders

    // Primitive methods cannot be inlined, so there's no point in building a MethodInlineInfo. Also, some
    // primitive methods (e.g., `isInstanceOf`) have non-erased types, which confuses [[typeToBType]].
    val methodInlineInfos = new collection.mutable.TreeMap[(String, String), MethodInlineInfo]()
    methods.foreach {
      methodSym =>
        val name = methodSym.javaSimpleName // same as in genDefDef
        val signature = (name, ts.asmMethodType(methodSym).descriptor)

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
   * Build the InlineInfo for a class. For Scala classes, the information is stored in the
   * ScalaInlineInfo attribute. If the attribute is missing, the InlineInfo is built using the
   * metadata available in the classfile (ACC_FINAL flags, etc.).
   */
  def inlineInfoFromClassfile(classNode: ClassNode, moduleNode: Option[ModuleNode]): InlineInfo = {
    def fromClassfileAttribute: Option[InlineInfo] = {
      if (classNode.attrs == null) None
      else classNode.attrs.asScala.collectFirst { case a: InlineInfoAttribute => a.inlineInfo }
    }

    def fromClassfileWithoutAttribute = {
      val warning = {
        val isScala = classNode.attrs != null && classNode.attrs.asScala.exists(a => a.`type` == nme.ScalaATTR.toString || a.`type` == nme.ScalaSignatureATTR.toString)
        if (isScala) Some(NoInlineInfoAttribute(classNode.name))
        else None
      }
      // when building MethodInlineInfos for the members of a ClassSymbol, we exclude those methods
      // in scalaPrimitives. This is necessary because some of them have non-erased types, which would
      // require special handling. Excluding is OK because they are never inlined.
      // Here we are parsing from a classfile and we don't need to do anything special. Many of these
      // primitives don't even exist, for example Any.isInstanceOf.
      val methodInfos = new mutable.TreeMap[(String, String), MethodInlineInfo]()
      val isFinalClass = BCodeUtils.isFinalClass(classNode)
      classNode.methods.forEach(methodNode => {
        val info = MethodInlineInfo(
          effectivelyFinal = isFinalClass || BCodeUtils.isFinalMethod(methodNode),
          annotatedInline = false,
          annotatedNoInline = false)
        methodInfos((methodNode.name, methodNode.desc)) = info
      })

      // We only want an approximation of SAMs for inlining heuristics, no need to check FunctionalInterface annotations or such
      val abstractMethods = classNode.methods.asScala.filter(m => (m.access & ACC_ABSTRACT) != 0)
      val sam = if abstractMethods.size == 1 then Some(abstractMethods.head.name + abstractMethods.head.desc) else None

      // No module => always accessible
      val isAccessible = moduleNode.forall(m =>
        val nodePackageName = classNode.name.substring(0, classNode.name.lastIndexOf('/'))
        m.exports.asScala.exists(e => (e.modules == null || e.modules.size == 0) && e.packaze == nodePackageName)
      )

      InlineInfo(isFinalClass, sam, methodInfos, warning, isAccessible)
    }

    fromClassfileAttribute.getOrElse(fromClassfileWithoutAttribute)
  }
}

object InlineInfo {
  val empty = InlineInfo(isEffectivelyFinal = false, sam = None, methodInfos = SortedMap.empty, warning = None, isAccessible = false)

  /**
   * Check if a type is accessible to some class, as defined in JVMS 5.4.4.
   * (A1) C is public
   * (A2) C and D are members of the same run-time package
   */
  @tailrec
  def classIsAccessible(accessed: BType, from: ClassBType): Boolean = (accessed: @unchecked) match {
    // TODO: A2 requires "same run-time package", which seems to be package + classloader (JVMS 5.3.). is the below ok?
    case c: ClassBType => c.isPublic || c.packageInternalName == from.packageInternalName
    case a: ArrayBType => classIsAccessible(a.elementType, from)
    case _: PrimitiveBType => true
  }

  /**
   * Check if a member reference is accessible from the `destinationClass`, as defined in the
   * JVMS 5.4.4. Note that the class name in a field / method reference is not necessarily the
   * class in which the member is declared:
   *
   * class A { def f = 0 }; class B extends A { f }
   *
   * The INVOKEVIRTUAL instruction uses a method reference "B.f ()I". Therefore this method has
   * two parameters:
   *
   * @param memberDeclClass The class in which the member is declared (A)
   * @param memberRefClass  The class used in the member reference (B)
   *
   *                        (B0) JVMS 5.4.3.2 / 5.4.3.3: when resolving a member of class C in D, the class C is resolved
   *                        first. According to 5.4.3.1, this requires C to be accessible in D.
   *
   *                        JVMS 5.4.4 summary: A field or method R is accessible to a class D (destinationClass) iff
   *                        (B1) R is public
   *                        (B2) R is protected, declared in C (memberDeclClass) and D is a subclass of C.
   *                        If R is not static, R must contain a symbolic reference to a class T (memberRefClass),
   *                        such that T is either a subclass of D, a superclass of D, or D itself.
   *                        Also (P) needs to be satisfied.
   *                        (B3) R is either protected or has default access and declared by a class in the same
   *                        run-time package as D.
   *                        If R is protected, also (P) needs to be satisfied.
   *                        (B4) R is private and is declared in D.
   *
   *                        (P) When accessing a protected instance member, the target object on the stack (the receiver)
   *                        has to be a subtype of D (destinationClass). This is enforced by classfile verification
   *                        (https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.10.1.8).
   *
   *                        TODO: we cannot currently implement (P) because we don't have the necessary information
   *                        available. Once we have a type propagation analysis implemented, we can extract the receiver
   *                        type from there (https://github.com/scala-opt/scala/issues/13).
   */
  def memberIsAccessible(memberFlags: Int, memberDeclClass: ClassBType, memberRefClass: ClassBType, from: ClassBType): Boolean = {
    // TODO: B3 requires "same run-time package", which seems to be package + classloader (JVMS 5.3.). is the below ok?
    def samePackageAsDestination = memberDeclClass.packageInternalName == from.packageInternalName

    def targetObjectConformsToDestinationClass = false // needs type propagation analysis, see above

    def memberIsAccessibleImpl = {
      val key = (ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE) & memberFlags
      key match {
        case ACC_PUBLIC => // B1
          true

        case ACC_PROTECTED => // B2
          val isStatic = (ACC_STATIC & memberFlags) != 0
          val condB2 = from.isSubtypeOf(memberDeclClass) && {
            isStatic || memberRefClass.isSubtypeOf(from) || from.isSubtypeOf(memberRefClass)
          }
          (condB2 || samePackageAsDestination /* B3 (protected) */) &&
            (isStatic || targetObjectConformsToDestinationClass) // (P)


        case 0 => // B3 (default access)
          samePackageAsDestination

        case ACC_PRIVATE => // B4
          memberDeclClass == from
      }
    }

    classIsAccessible(memberDeclClass, from) && memberIsAccessibleImpl // B0
  }
}
