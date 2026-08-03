package dotty.tools
package dotc
package config

import io.*
import classpath.AggregateClassPath
import core.*
import Symbols.*, Types.*, Contexts.*, StdNames.*
import Flags.*
import interactive.LogicalPackage
import transform.ExplicitOuter

import scala.util.Properties.isJavaAtLeast

class JavaPlatform(precomputedSourcePackages: Option[LogicalPackage] = None) extends Platform {

  private var currentClassPath: Option[ClassPath] = None

  def classPath(using Context): ClassPath = {
    if (currentClassPath.isEmpty)
      currentClassPath = Some(new PathResolver(precomputedSourcePackages).result)
    val cp = currentClassPath.get
    cp
  }

  // The given symbol is a method with the right name and signature to be a runnable java program.
  def isMainMethod(sym: Symbol)(using Context): Boolean =
    sym.name == nme.main && isMainMethodType(sym.info, allowNoArgs = false)

  private def isMainMethodType(info: Type, allowNoArgs: Boolean)(using Context): Boolean =
    info match
      case MethodTpe(_, parameters, resultType) if resultType.isRef(defn.UnitClass) =>
        parameters match
          case defn.ArrayOf(elementType) :: Nil => elementType =:= defn.StringType
          case Nil => allowNoArgs
          case _ => false
      case ExprType(resultType) =>
        allowNoArgs && resultType.isRef(defn.UnitClass)
      case _ => false

  protected def supportsJava25MainClassDiscovery: Boolean = true

  override def isMainClass(sym: ClassSymbol)(using Context): Boolean =
    if supportsJava25MainClassDiscovery && isJavaAtLeast(25) then
      sym.isStatic && !sym.isOneOf(AbstractOrTrait) && hasJava25Main(sym)
    else super.isMainClass(sym)

  private def hasJava25Main(sym: ClassSymbol)(using Context): Boolean =
    sym.info.member(nme.main).hasAltWith: d =>
      val main = d.symbol
      !main.is(Private)
        && isMainMethodType(main.info, allowNoArgs = true)
        && isInvocableJava25Main(sym, main)

  private def isInvocableJava25Main(owner: ClassSymbol, main: Symbol)(using Context): Boolean =
    if isEmittedAsStaticMethod(main) then true
    else if owner.is(Module) then main.isPublic
    else hasInvocableNoArgConstructor(owner)

  private def isEmittedAsStaticMethod(sym: Symbol)(using Context): Boolean =
    sym.is(JavaStatic) || sym.isScalaStatic

  private def hasInvocableNoArgConstructor(sym: ClassSymbol)(using Context): Boolean =
    sym.info.member(nme.CONSTRUCTOR).hasAltWith: d =>
      val constructor = d.symbol
      !constructor.is(Private) && !constructor.info.takesParams

  def addToClassPath(cPath: ClassPath)(using Context): Unit = classPath match {
    case AggregateClassPath(entries) =>
      currentClassPath = Some(AggregateClassPath(entries :+ cPath))
    case cp: ClassPath =>
      currentClassPath = Some(AggregateClassPath(cp :: cPath :: Nil))
  }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit = currentClassPath.get match {
    case AggregateClassPath(entries) =>
      currentClassPath = Some(AggregateClassPath(entries map (e => subst.getOrElse(e, e))))
    case cp: ClassPath =>
      currentClassPath = Some(subst.getOrElse(cp, cp))
  }

  def rootLoader(root: TermSymbol)(using Context): SymbolLoader = new SymbolLoaders.PackageLoader(root, classPath)

  /** Is the SAMType `cls` also a SAM under the rules of the JVM? */
  def isSam(cls: ClassSymbol)(using Context): Boolean =
    cls.isAllOf(NoInitsTrait) &&
    cls.superClass == defn.ObjectClass &&
    cls.directlyInheritedTraits.forall(_.is(NoInits)) &&
    !ExplicitOuter.needsOuterIfReferenced(cls) &&
    // Superaccessors already show up as abstract methods here, so no test necessary
    cls.typeRef.fields.isEmpty &&
    // Check if the SAM can be implemented via LambdaMetaFactory
    TypeErasure.samExpansionNotNeeded(cls)

  /** We could get away with excluding BoxedBooleanClass for the
   *  purpose of equality testing since it need not compare equal
   *  to anything but other booleans, but it should be present in
   *  case this is put to other uses.
   */
  def isMaybeBoxed(sym: ClassSymbol)(using Context): Boolean = {
    val d = defn
    import d.*
    sym == ObjectClass
    || sym == JavaSerializableClass
    || sym == ComparableClass
    || sym.derivesFrom(BoxedNumberClass)
    || sym.derivesFrom(BoxedCharClass)
    || sym.derivesFrom(BoxedBooleanClass)
  }

  def shouldReceiveJavaSerializationMethods(sym: ClassSymbol)(using Context): Boolean =
    true

  def newClassLoader(bin: AbstractFile)(using Context): SymbolLoader =
    new ClassfileLoader(bin)

  def newTastyLoader(bin: AbstractFile)(using Context): SymbolLoader =
    new TastyLoader(bin)

  def typeMightBeSubtypeAtRuntime(c: Symbol, potentialSuperClass: Symbol)(using Context): Boolean =
    // On the JVM, we add an implementation of Serializable to everything
    potentialSuperClass == defn.JavaSerializableClass

  def supportsSynchronizedMethods: Boolean = true
}
