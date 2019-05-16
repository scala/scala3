package dotty.tools
package dotc
package core
package unpickleScala2

import java.io.IOException
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import Contexts._, Symbols._, Types._, Scopes._, SymDenotations._, Names._, NameOps._
import StdNames._, Denotations._, NameOps._, Flags._, Constants._, Annotations._
import NameKinds.{Scala2MethodNameKinds, SuperAccessorName, ExpandedName}
import util.Spans._
import dotty.tools.dotc.ast.{tpd, untpd}, ast.tpd._
import ast.untpd.Modifiers
import printing.Texts._
import printing.Printer
import io.AbstractFile
import util.common._
import typer.Checking.checkNonCyclic
import transform.SymUtils._
import PickleBuffer._
import PickleFormat._
import Decorators._
import TypeApplications._
import classfile.ClassfileParser
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.annotation.switch
import reporting.trace
import dotty.tools.dotc.reporting.diagnostic.messages.FailureToEliminateExistential

object Scala2Unpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)

  case class TempPolyType(tparams: List[TypeSymbol], tpe: Type) extends UncachedGroundType {
    override def fallbackToText(printer: Printer): Text =
      "[" ~ printer.dclsText(tparams, ", ") ~ "]" ~ printer.toText(tpe)
  }

  /** Temporary type for classinfos, will be decomposed on completion of the class */
  case class TempClassInfoType(parentTypes: List[Type], decls: Scope, clazz: Symbol) extends UncachedGroundType

  /** Convert temp poly type to poly type and leave other types alone. */
  def translateTempPoly(tp: Type)(implicit ctx: Context): Type = tp match {
    case TempPolyType(tparams, restpe) =>
      // This check used to read `owner.isTerm` but that wasn't always correct,
      // I'm not sure `owner.is(Method)` is 100% correct either but it seems to
      // work better. See the commit message where this change was introduced
      // for more information.
      (if (tparams.head.owner.is(Method)) PolyType else HKTypeLambda)
        .fromParams(tparams, restpe)
    case tp => tp
  }

  def addConstructorTypeParams(denot: SymDenotation)(implicit ctx: Context): Unit = {
    assert(denot.isConstructor)
    denot.info = PolyType.fromParams(denot.owner.typeParams, denot.info)
  }

  /** Convert array parameters denoting a repeated parameter of a Java method
   *  to `RepeatedParamClass` types.
   */
  def arrayToRepeated(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: MethodType =>
      val lastArg = tp.paramInfos.last
      assert(lastArg isRef defn.ArrayClass)
      val elemtp0 :: Nil = lastArg.baseType(defn.ArrayClass).argInfos
      val elemtp = elemtp0 match {
        case AndType(t1, t2) => // drop intersection with Object for abstract types an parameters in varargs. Erasure can handle them.
          if (t2.isRef(defn.ObjectClass))
            t1 match {
              case t1: TypeParamRef => t1
              case t1: TypeRef if t1.symbol.isAbstractOrParamType => t1
              case _ => elemtp0
            }
          else elemtp0
        case _ =>
          elemtp0
      }
      tp.derivedLambdaType(
        tp.paramNames,
        tp.paramInfos.init :+ defn.RepeatedParamType.appliedTo(elemtp),
        tp.resultType)
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, arrayToRepeated(tp.resultType))
  }

  def ensureConstructor(cls: ClassSymbol, scope: Scope)(implicit ctx: Context): Unit =
    if (scope.lookup(nme.CONSTRUCTOR) == NoSymbol) {
      val constr = ctx.newDefaultConstructor(cls)
      addConstructorTypeParams(constr)
      cls.enter(constr, scope)
    }

  def setClassInfo(denot: ClassDenotation, info: Type, fromScala2: Boolean, selfInfo: Type = NoType)(implicit ctx: Context): Unit = {
    val cls = denot.classSymbol
    val (tparams, TempClassInfoType(parents, decls, clazz)) = info match {
      case TempPolyType(tps, cinfo) => (tps, cinfo)
      case cinfo => (Nil, cinfo)
    }
    val ost =
      if ((selfInfo eq NoType) && (denot is ModuleClass) && denot.sourceModule.exists)
        // it seems sometimes the source module does not exist for a module class.
        // An example is `scala.reflect.internal.Trees.Template$. Without the
        // `denot.sourceModule.exists` provision i859.scala crashes in the backend.
        denot.owner.thisType select denot.sourceModule
      else selfInfo
    val tempInfo = new TempClassInfo(denot.owner.thisType, cls, decls, ost)
    denot.info = tempInfo // first rough info to avoid CyclicReferences
    val parents1 = if (parents.isEmpty) defn.ObjectType :: Nil else parents.map(_.dealias)
    // Add extra parents to the tuple classes from the standard library
    val normalizedParents =
      if (fromScala2) defn.adjustForTuple(cls, tparams, parents1)
      else parents1 // We are setting the info of a Java class, so it cannot be one of the tuple classes
    for (tparam <- tparams) {
      val tsym = decls.lookup(tparam.name)
      if (tsym.exists) tsym.setFlag(TypeParam)
      else denot.enter(tparam, decls)
    }
    if (!(denot.flagsUNSAFE is JavaModule)) ensureConstructor(denot.symbol.asClass, decls)

    val scalacCompanion = denot.classSymbol.scalacLinkedClass

    def registerCompanionPair(module: Symbol, claz: Symbol) = {
      module.registerCompanion(claz)
      claz.registerCompanion(module)
    }

    if (denot.flagsUNSAFE is Module)
      registerCompanionPair(denot.classSymbol, scalacCompanion)
    else
      registerCompanionPair(scalacCompanion, denot.classSymbol)

    tempInfo.finalize(denot, normalizedParents, ost) // install final info, except possibly for typeparams ordering
    denot.ensureTypeParamsInCorrectOrder()
  }
}

/** Unpickle symbol table information descending from a class and/or module root
 *  from an array of bytes.
 *  @param bytes      bytearray from which we unpickle
 *  @param classroot  the top-level class which is unpickled, or NoSymbol if inapplicable
 *  @param moduleroot the top-level module class which is unpickled, or NoSymbol if inapplicable
 *  @param filename   filename associated with bytearray, only used for error messages
 */
class Scala2Unpickler(bytes: Array[Byte], classRoot: ClassDenotation, moduleClassRoot: ClassDenotation)(ictx: Context)
  extends PickleBuffer(bytes, 0, -1) with ClassfileParser.Embedded {

  def showPickled(): Unit = {
    atReadPos(0, () => {
      println(s"classRoot = ${classRoot.debugString}, moduleClassRoot = ${moduleClassRoot.debugString}")
      util.ShowPickled.printFile(this)
    })
  }

  // print("unpickling "); showPickled() // !!! DEBUG

  import Scala2Unpickler._

  val moduleRoot: SymDenotation = moduleClassRoot.sourceModule(ictx).denot(ictx)
  assert(moduleRoot.isTerm)

  checkVersion(ictx)

  private val loadingMirror = defn(ictx) // was: mirrorThatLoaded(classRoot)

  /** A map from entry numbers to array offsets */
  private val index = createIndex

  /** A map from entry numbers to symbols, types, or annotations */
  private val entries = new Array[AnyRef](index.length)

  /** A map from symbols to their associated `decls` scopes */
  private val symScopes = mutable.AnyRefMap[Symbol, Scope]()

  protected def errorBadSignature(msg: String, original: Option[RuntimeException] = None)(implicit ctx: Context): Nothing = {
    val ex = new BadSignature(
      i"""error reading Scala signature of $classRoot from $source:
         |error occurred at position $readIndex: $msg""")
    if (ctx.settings.YdebugMissingRefs.value) original.getOrElse(ex).printStackTrace()
    throw ex
  }

  protected def handleRuntimeException(ex: RuntimeException)(implicit ctx: Context): Nothing = ex match {
    case ex: BadSignature => throw ex
    case _ => errorBadSignature(s"a runtime exception occurred: $ex", Some(ex))
  }

  def run()(implicit ctx: Context): Unit =
    try {
      var i = 0
      while (i < index.length) {
        if (entries(i) == null && isSymbolEntry(i)) {
          val savedIndex = readIndex
          readIndex = index(i)
          val sym = readSymbol()
          entries(i) = sym
          sym.infoOrCompleter match {
            case info: ClassUnpickler => info.init()
            case _ =>
          }
          readIndex = savedIndex
        }
        i += 1
      }
      // read children last, fix for #3951
      i = 0
      while (i < index.length) {
        if (entries(i) == null) {
          if (isSymbolAnnotationEntry(i)) {
            val savedIndex = readIndex
            readIndex = index(i)
            readSymbolAnnotation()
            readIndex = savedIndex
          } else if (isChildrenEntry(i)) {
            val savedIndex = readIndex
            readIndex = index(i)
            readChildren()
            readIndex = savedIndex
          }
        }
        i += 1
      }
    } catch {
      case ex: RuntimeException => handleRuntimeException(ex)
    }

  def source(implicit ctx: Context): AbstractFile = {
    val f = classRoot.symbol.associatedFile
    if (f != null) f else moduleClassRoot.symbol.associatedFile
  }

  private def checkVersion(implicit ctx: Context): Unit = {
    val major = readNat()
    val minor = readNat()
    if (major != MajorVersion || minor > MinorVersion)
      throw new IOException("Scala signature " + classRoot.fullName +
        " has wrong version\n expected: " +
        MajorVersion + "." + MinorVersion +
        "\n found: " + major + "." + minor +
        " in " + source)
  }

  /** The `decls` scope associated with given symbol */
  protected def symScope(sym: Symbol): Scope = symScopes.getOrElseUpdate(sym, newScope)

  /** Does entry represent an (internal) symbol */
  protected def isSymbolEntry(i: Int)(implicit ctx: Context): Boolean = {
    val tag = bytes(index(i)).toInt
    (firstSymTag <= tag && tag <= lastSymTag &&
      (tag != CLASSsym || !isRefinementSymbolEntry(i)))
  }

  /** Does entry represent an (internal or external) symbol */
  protected def isSymbolRef(i: Int): Boolean = {
    val tag = bytes(index(i))
    (firstSymTag <= tag && tag <= lastExtSymTag)
  }

  /** Does entry represent a name? */
  protected def isNameEntry(i: Int): Boolean = {
    val tag = bytes(index(i)).toInt
    tag == TERMname || tag == TYPEname
  }

  /** Does entry represent a symbol annotation? */
  protected def isSymbolAnnotationEntry(i: Int): Boolean = {
    val tag = bytes(index(i)).toInt
    tag == SYMANNOT
  }

  /** Does the entry represent children of a symbol? */
  protected def isChildrenEntry(i: Int): Boolean = {
    val tag = bytes(index(i)).toInt
    tag == CHILDREN
  }

  /** Does entry represent a refinement symbol?
   *  pre: Entry is a class symbol
   */
  protected def isRefinementSymbolEntry(i: Int)(implicit ctx: Context): Boolean = {
    val savedIndex = readIndex
    readIndex = index(i)
    val tag = readByte().toInt
    assert(tag == CLASSsym)

    readNat(); // read length
    val result = readNameRef() == tpnme.REFINE_CLASS
    readIndex = savedIndex
    result
  }

  protected def isRefinementClass(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.name == tpnme.REFINE_CLASS

  protected def isLocal(sym: Symbol)(implicit ctx: Context): Boolean = isUnpickleRoot(sym.topLevelClass)

  protected def isUnpickleRoot(sym: Symbol)(implicit ctx: Context): Boolean = {
    val d = sym.denot
    d == moduleRoot || d == moduleClassRoot || d == classRoot
  }

  /** If entry at <code>i</code> is undefined, define it by performing
   *  operation <code>op</code> with <code>readIndex at start of i'th
   *  entry. Restore <code>readIndex</code> afterwards.
   */
  protected def at[T <: AnyRef](i: Int, op: () => T): T = {
    var r = entries(i)
    if (r eq null) {
      r = atReadPos(index(i), op)
      assert(entries(i) eq null, entries(i))
      entries(i) = r
    }
    r.asInstanceOf[T]
  }

  protected def atReadPos[T](start: Int, op: () => T): T = {
    val savedIndex = readIndex
    readIndex = start
    try op()
    finally readIndex = savedIndex
  }

  /** Read a name */
  protected def readName()(implicit ctx: Context): Name = {
    val tag = readByte()
    val len = readNat()
    tag match {
      case TERMname => termName(bytes, readIndex, len)
      case TYPEname => typeName(bytes, readIndex, len)
      case _ => errorBadSignature("bad name tag: " + tag)
    }
  }
  protected def readTermName()(implicit ctx: Context): TermName = readName().toTermName
  protected def readTypeName()(implicit ctx: Context): TypeName = readName().toTypeName

  /** Read a symbol */
  protected def readSymbol()(implicit ctx: Context): Symbol = readDisambiguatedSymbol(alwaysTrue)()

  /** Read a symbol, with possible disambiguation */
  protected def readDisambiguatedSymbol(p: Symbol => Boolean)()(implicit ctx: Context): Symbol = {
    val start = indexCoord(readIndex)
    val tag = readByte()
    val end = readNat() + readIndex
    def atEnd = readIndex == end

    def readExtSymbol(): Symbol = {
      val name = readNameRef().decode
      val owner = if (atEnd) loadingMirror.RootClass else readSymbolRef()

      def adjust(denot: Denotation) = {
        val denot1 = denot.disambiguate(p)
        val sym = denot1.symbol
        if (denot.exists && !denot1.exists) { // !!!DEBUG
          val alts = denot.alternatives map (d => d + ":" + d.info + "/" + d.signature)
          System.err.println(s"!!! disambiguation failure: $alts")
          val members = denot.alternatives.head.symbol.owner.info.decls.toList map (d => d + ":" + d.info + "/" + d.signature)
          System.err.println(s"!!! all members: $members")
        }
        if (tag == EXTref) sym else sym.moduleClass
      }

      def fromName(name: Name): Symbol = name.toTermName match {
        case nme.ROOT => loadingMirror.RootClass
        case nme.ROOTPKG => loadingMirror.RootPackage
        case _ =>
          def declIn(owner: Symbol) = adjust(owner.info.decl(name))
          val sym = declIn(owner)
          if (sym.exists || owner.ne(defn.ObjectClass)) sym else declIn(defn.AnyClass)
      }

      def slowSearch(name: Name): Symbol =
        owner.info.decls.find(_.name == name)

      def nestedObjectSymbol: Symbol = {
        // If the owner is overloaded (i.e. a method), it's not possible to select the
        // right member, so return NoSymbol. This can only happen when unpickling a tree.
        // the "case Apply" in readTree() takes care of selecting the correct alternative
        //  after parsing the arguments.
        //if (owner.isOverloaded)
        //  return NoSymbol

        if (tag == EXTMODCLASSref) {
          val module = owner.info.decl(name.toTermName).suchThat(_ is Module)
          module.info // force it, as completer does not yet point to module class.
          module.symbol.moduleClass

          /* was:
            val moduleVar = owner.info.decl(name.toTermName.moduleVarName).symbol
            if (moduleVar.isLazyAccessor)
              return moduleVar.lazyAccessor.lazyAccessor
           */
        } else NoSymbol
      }

      // println(s"read ext symbol $name from ${owner.denot.debugString} in ${classRoot.debugString}")  // !!! DEBUG

      // (1) Try name.
      fromName(name) orElse {
        // (2) Try with expanded name.  Can happen if references to private
        // symbols are read from outside: for instance when checking the children
        // of a class.  See #1722.
        fromName(name.toTermName.expandedName(owner)) orElse {
          // (3) Try as a nested object symbol.
          nestedObjectSymbol orElse {
            // (4) Call the mirror's "missing" hook.
            adjust(ctx.base.missingHook(owner, name)) orElse {
              // println(owner.info.decls.toList.map(_.debugString).mkString("\n  ")) // !!! DEBUG
              //              }
              // (5) Create a stub symbol to defer hard failure a little longer.
              System.err.println(i"***** missing reference, looking for ${name.debugString} in $owner")
              System.err.println(i"decls = ${owner.info.decls}")
              owner.info.decls.checkConsistent()
              if (slowSearch(name).exists)
                System.err.println(i"**** slow search found: ${slowSearch(name)}")
              if (ctx.settings.YdebugMissingRefs.value) Thread.dumpStack()
              ctx.newStubSymbol(owner, name, source)
            }
          }
        }
      }
    }

    tag match {
      case NONEsym => return NoSymbol
      case EXTref | EXTMODCLASSref => return readExtSymbol()
      case _ =>
    }

    // symbols that were pickled with Pickler.writeSymInfo
    val nameref = readNat()
    var name = at(nameref, () => readName()(ctx))
    val owner = readSymbolRef()

    var flags = unpickleScalaFlags(readLongNat(), name.isTypeName)
    if (flags is DefaultParameter) {
      // DefaultParameterized flag now on method, not parameter
      //assert(flags is Param, s"$name0 in $owner")
      flags = flags &~ DefaultParameterized
      owner.setFlag(DefaultParameterized)
    }

    name = name.adjustIfModuleClass(flags)
    if (flags is Method) {
      name =
        if (name == nme.TRAIT_CONSTRUCTOR) nme.CONSTRUCTOR
        else name.asTermName.unmangle(Scala2MethodNameKinds)
    }
    if ((flags is Scala2ExpandedName)) {
      name = name.unmangle(ExpandedName)
      flags = flags &~ Scala2ExpandedName
    }
    if (flags is Scala2SuperAccessor) {
      name = name.asTermName.unmangle(SuperAccessorName)
      flags = flags &~ Scala2SuperAccessor
    }
    name = name.mapLast(_.decode)

    def nameMatches(rootName: Name) = name == rootName
    def isClassRoot = nameMatches(classRoot.name) && (owner == classRoot.owner) && !(flags is ModuleClass)
    def isModuleClassRoot = nameMatches(moduleClassRoot.name) && (owner == moduleClassRoot.owner) && (flags is Module)
    def isModuleRoot = nameMatches(moduleClassRoot.name.sourceModuleName) && (owner == moduleClassRoot.owner) && (flags is Module)

    //if (isClassRoot) println(s"classRoot of $classRoot found at $readIndex, flags = $flags") // !!! DEBUG
    //if (isModuleRoot) println(s"moduleRoot of $moduleRoot found at $readIndex, flags = $flags") // !!! DEBUG
    //if (isModuleClassRoot) println(s"moduleClassRoot of $moduleClassRoot found at $readIndex, flags = $flags") // !!! DEBUG

    def completeRoot(denot: ClassDenotation, completer: LazyType): Symbol = {
      denot.setFlag(flags)
      denot.resetFlag(Touched) // allow one more completion

      // Temporary measure, as long as we do not read these classes from Tasty.
      // Scala-2 classes don't have NoInits set even if they are pure. We override this
      // for Product and Serializable so that case classes can be pure. A full solution
      // requires that we read all Scala code from Tasty.
      if (owner == defn.ScalaPackageClass && ((name eq tpnme.Serializable) || (name eq tpnme.Product)))
        denot.setFlag(NoInits)

      denot.info = completer
      denot.symbol
    }

    def finishSym(sym: Symbol): Symbol = {
      if (sym.isClass) {
        sym.setFlag(Scala2x)
      }
      if (!(isRefinementClass(sym) || isUnpickleRoot(sym) || (sym is Scala2Existential))) {
        val owner = sym.owner
        if (owner.isClass)
          owner.asClass.enter(sym, symScope(owner))
        else if (isRefinementClass(owner))
          symScope(owner).openForMutations.enter(sym)
      }
      sym
    }

    finishSym(tag match {
      case TYPEsym | ALIASsym =>
        var name1 = name.asTypeName
        var flags1 = flags
        if (flags is TypeParam) flags1 |= owner.typeParamCreationFlags
        ctx.newSymbol(owner, name1, flags1, localMemberUnpickler, coord = start)
      case CLASSsym =>
        var infoRef = readNat()
        if (isSymbolRef(infoRef)) infoRef = readNat()
        if (isClassRoot)
          completeRoot(
            classRoot, rootClassUnpickler(start, classRoot.symbol, NoSymbol, infoRef))
        else if (isModuleClassRoot)
          completeRoot(
            moduleClassRoot, rootClassUnpickler(start, moduleClassRoot.symbol, moduleClassRoot.sourceModule, infoRef))
        else if (name == tpnme.REFINE_CLASS)
          // create a type alias instead
          ctx.newSymbol(owner, name, flags, localMemberUnpickler, coord = start)
        else {
          def completer(cls: Symbol) = {
            val unpickler = new ClassUnpickler(infoRef) withDecls symScope(cls)
            if (flags is ModuleClass)
              unpickler withSourceModule (implicit ctx =>
                cls.owner.info.decls.lookup(cls.name.sourceModuleName)
                  .suchThat(_ is Module).symbol)
            else unpickler
          }
          ctx.newClassSymbol(owner, name.asTypeName, flags, completer, coord = start)
        }
      case VALsym =>
        ctx.newSymbol(owner, name.asTermName, flags, localMemberUnpickler, coord = start)
      case MODULEsym =>
        if (isModuleRoot) {
          moduleRoot setFlag flags
          moduleRoot.symbol
        } else ctx.newSymbol(owner, name.asTermName, flags,
          new LocalUnpickler() withModuleClass(implicit ctx =>
            owner.info.decls.lookup(name.moduleClassName)
              .suchThat(_ is Module).symbol)
          , coord = start)
      case _ =>
        errorBadSignature("bad symbol tag: " + tag)
    })
  }

  class LocalUnpickler extends LazyType {
    def startCoord(denot: SymDenotation): Coord = denot.symbol.coord
    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = try {
      def parseToCompletion(denot: SymDenotation)(implicit ctx: Context) = {
        val tag = readByte()
        val end = readNat() + readIndex
        def atEnd = readIndex == end
        val unusedNameref = readNat()
        val unusedOwnerref = readNat()
        val unusedFlags = readLongNat()
        var inforef = readNat()
        denot.privateWithin =
          if (!isSymbolRef(inforef)) NoSymbol
          else {
            val pw = at(inforef, () => readSymbol())
            inforef = readNat()
            pw
          }
        // println("reading type for " + denot) // !!! DEBUG
        val tp = at(inforef, () => readType()(ctx))
        denot match {
          case denot: ClassDenotation =>
            val selfInfo = if (atEnd) NoType else readTypeRef()
            setClassInfo(denot, tp, fromScala2 = true, selfInfo)
          case denot =>
            val tp1 = translateTempPoly(tp)
            denot.info =
              if (tag == ALIASsym) TypeAlias(tp1)
              else if (denot.isType) checkNonCyclic(denot.symbol, tp1, reportErrors = false)
                // we need the checkNonCyclic call to insert LazyRefs for F-bounded cycles
              else if (!denot.is(Param)) tp1.underlyingIfRepeated(isJava = false)
              else tp1
            if (denot.isConstructor) addConstructorTypeParams(denot)
            if (atEnd) {
              assert(!denot.isSuperAccessor, denot)
            } else {
              assert(denot.is(ParamAccessor) || denot.isSuperAccessor, denot)
              def disambiguate(alt: Symbol) = { // !!! DEBUG
                trace.onDebug(s"disambiguating ${denot.info} =:= ${denot.owner.thisType.memberInfo(alt)} ${denot.owner}") {
                  denot.info matches denot.owner.thisType.memberInfo(alt)
                }
              }
              val alias = readDisambiguatedSymbolRef(disambiguate).asTerm
              denot.addAnnotation(Annotation.makeAlias(alias))
            }
        }
        // println(s"unpickled ${denot.debugString}, info = ${denot.info}") !!! DEBUG
      }
      atReadPos(startCoord(denot).toIndex,
          () => parseToCompletion(denot)(
            ctx.addMode(Mode.Scala2Unpickling).withPhaseNoLater(ctx.picklerPhase)))
    } catch {
      case ex: RuntimeException => handleRuntimeException(ex)
    }
  }

  object localMemberUnpickler extends LocalUnpickler

  class ClassUnpickler(infoRef: Int) extends LocalUnpickler with TypeParamsCompleter {
    private def readTypeParams()(implicit ctx: Context): List[TypeSymbol] = {
      val tag = readByte()
      val end = readNat() + readIndex
      if (tag == POLYtpe) {
        val unusedRestpeRef = readNat()
        until(end, () => readSymbolRef()(ctx)).asInstanceOf[List[TypeSymbol]]
      } else Nil
    }
    private def loadTypeParams(implicit ctx: Context) =
      atReadPos(index(infoRef), () => readTypeParams()(ctx))

    /** Force reading type params early, we need them in setClassInfo of subclasses. */
    def init()(implicit ctx: Context): List[TypeSymbol] = loadTypeParams

    override def completerTypeParams(sym: Symbol)(implicit ctx: Context): List[TypeSymbol] =
      loadTypeParams
  }

  def rootClassUnpickler(start: Coord, cls: Symbol, module: Symbol, infoRef: Int): ClassUnpickler =
    (new ClassUnpickler(infoRef) with SymbolLoaders.SecondCompleter {
      override def startCoord(denot: SymDenotation): Coord = start
    }) withDecls symScope(cls) withSourceModule (_ => module)

  /** Convert
   *    tp { type name = sym } forSome { sym >: L <: H }
   *  to
   *    tp { name >: L <: H }
   *  and
   *    tp { name: sym } forSome { sym <: T with Singleton }
   *  to
   *    tp { name: T }
   */
  def elimExistentials(boundSyms: List[Symbol], tp: Type)(implicit ctx: Context): Type = {
    // Need to be careful not to run into cyclic references here (observed when
    // compiling t247.scala). That's why we avoid taking `symbol` of a TypeRef
    // unless names match up.
    val isBound = (tp: Type) => {
      def refersTo(tp: Type, sym: Symbol): Boolean = tp match {
        case tp: TypeRef => sym.name == tp.name && sym == tp.symbol
        case tp: TypeVar => refersTo(tp.underlying, sym)
        case tp : LazyRef => refersTo(tp.ref, sym)
        case _ => false
      }
      boundSyms.exists(refersTo(tp, _))
    }
    // Cannot use standard `existsPart` method because it calls `lookupRefined`
    // which can cause CyclicReference errors.
    val isBoundAccumulator = new ExistsAccumulator(isBound) {
      override def foldOver(x: Boolean, tp: Type): Boolean = tp match {
        case tp: TypeRef => applyToPrefix(x, tp)
        case _ => super.foldOver(x, tp)
      }
    }
    def removeSingleton(tp: Type): Type =
      if (tp isRef defn.SingletonClass) defn.AnyType else tp
    def elim(tp: Type): Type = tp match {
      case tp @ RefinedType(parent, name, rinfo) =>
        val parent1 = elim(tp.parent)
        rinfo match {
          case TypeAlias(info: TypeRef) if isBound(info) =>
            RefinedType(parent1, name, info.symbol.info)
          case info: TypeRef if isBound(info) =>
            val info1 = info.symbol.info
            assert(info1.derivesFrom(defn.SingletonClass))
            RefinedType(parent1, name, info1.mapReduceAnd(removeSingleton)(_ & _))
          case info =>
            tp.derivedRefinedType(parent1, name, info)
        }
      case tp @ AppliedType(tycon, args) =>
        val tycon1 = tycon.safeDealias
        def mapArg(arg: Type) = arg match {
          case arg: TypeRef if isBound(arg) => arg.symbol.info
          case _ => arg
        }
        if (tycon1 ne tycon) elim(tycon1.appliedTo(args))
        else tp.derivedAppliedType(tycon, args.map(mapArg))
      case _ =>
        tp
    }
    val tp1 = elim(tp)
    if (isBoundAccumulator(false, tp1)) {
      val anyTypes = boundSyms map (_ => defn.AnyType)
      val boundBounds = boundSyms map (_.info.bounds.hi)
      val tp2 = tp1.subst(boundSyms, boundBounds).subst(boundSyms, anyTypes)
      ctx.warning(FailureToEliminateExistential(tp, tp1, tp2, boundSyms))
      tp2
    } else tp1
  }

  /** Read a type
   *
   *  @param forceProperType is used to ease the transition to NullaryMethodTypes (commentmarker: NMT_TRANSITION)
   *        the flag say that a type of kind * is expected, so that PolyType(tps, restpe) can be disambiguated to PolyType(tps, NullaryMethodType(restpe))
   *        (if restpe is not a ClassInfoType, a MethodType or a NullaryMethodType, which leaves TypeRef/SingletonType -- the latter would make the polytype a type constructor)
   */
  protected def readType()(implicit ctx: Context): Type = {
    val tag = readByte()
    val end = readNat() + readIndex
    (tag: @switch) match {
      case NOtpe =>
        NoType
      case NOPREFIXtpe =>
        NoPrefix
      case THIStpe =>
        readSymbolRef().thisType
      case SINGLEtpe =>
        val pre = readTypeRef()
        val sym = readDisambiguatedSymbolRef(_.info.isParameterless)
        pre.select(sym)
      case SUPERtpe =>
        val thistpe = readTypeRef()
        val supertpe = readTypeRef()
        SuperType(thistpe, supertpe)
      case CONSTANTtpe =>
        ConstantType(readConstantRef())
      case TYPEREFtpe =>
        var pre = readTypeRef()
        val sym = readSymbolRef()
        pre match {
          case thispre: ThisType =>
            // The problem is that class references super.C get pickled as
            // this.C. Dereferencing the member might then get an overriding class
            // instance. The problem arises for instance for LinkedHashMap#MapValues
            // and also for the inner Transform class in all views. We fix it by
            // replacing the this with the appropriate super.
            if (sym.owner != thispre.cls) {
              val overriding = thispre.cls.info.decls.lookup(sym.name)
              if (overriding.exists && overriding != sym) {
                val base = pre.baseType(sym.owner)
                assert(base.exists)
                pre = SuperType(thispre, base)
              }
            }
          case NoPrefix if sym is TypeParam =>
            pre = sym.owner.thisType
          case _ =>
        }
        val tycon = pre.select(sym)
        val args = until(end, () => readTypeRef())
        if (sym == defn.ByNameParamClass2x) ExprType(args.head)
        else if (args.nonEmpty) tycon.safeAppliedTo(EtaExpandIfHK(sym.typeParams, args.map(translateTempPoly)))
        else if (sym.typeParams.nonEmpty) tycon.EtaExpand(sym.typeParams)
        else tycon
      case TYPEBOUNDStpe =>
        TypeBounds(readTypeRef(), readTypeRef())
      case REFINEDtpe =>
        val clazz = readSymbolRef()
        val decls = symScope(clazz)
        symScopes(clazz) = EmptyScope // prevent further additions
        val parents = until(end, () => readTypeRef())
        val parent = parents.reduceLeft(AndType(_, _))
        if (decls.isEmpty) parent
        else {
          def subst(info: Type, rt: RecType) =
            if (clazz.isClass) info.substThis(clazz.asClass, rt.recThis)
            else info // turns out some symbols read into `clazz` are not classes, not sure why this is the case.
          def addRefinement(tp: Type, sym: Symbol) = RefinedType(tp, sym.name, sym.info)
          val refined = (parent /: decls.toList)(addRefinement)
          RecType.closeOver(rt => subst(refined, rt))
        }
      case CLASSINFOtpe =>
        val clazz = readSymbolRef()
        TempClassInfoType(until(end, () => readTypeRef()), symScope(clazz), clazz)
      case METHODtpe | IMPLICITMETHODtpe =>
        val restpe = readTypeRef()
        val params = until(end, () => readSymbolRef())
        val maker = MethodType.companion(
          isImplicit = tag == IMPLICITMETHODtpe || params.nonEmpty && params.head.is(Implicit))
        maker.fromSymbols(params, restpe)
      case POLYtpe =>
        val restpe = readTypeRef()
        val typeParams = until(end, () => readSymbolRef())
        if (typeParams.nonEmpty) TempPolyType(typeParams.asInstanceOf[List[TypeSymbol]], restpe.widenExpr)
        else ExprType(restpe)
      case EXISTENTIALtpe =>
        val restpe = readTypeRef()
        val boundSyms = until(end, () => readSymbolRef())
        elimExistentials(boundSyms, restpe)
      case ANNOTATEDtpe =>
        AnnotatedType.make(readTypeRef(), until(end, () => readAnnotationRef()))
      case _ =>
        noSuchTypeTag(tag, end)
    }
  }

  def readTypeParams()(implicit ctx: Context): List[Symbol] = {
    val tag = readByte()
    val end = readNat() + readIndex
    if (tag == POLYtpe) {
      val unusedRestperef = readNat()
      until(end, () => readSymbolRef())
    } else Nil
  }

  def noSuchTypeTag(tag: Int, end: Int)(implicit ctx: Context): Type =
    errorBadSignature("bad type tag: " + tag)

  /** Read a constant */
  protected def readConstant()(implicit ctx: Context): Constant = {
    val tag = readByte().toInt
    val len = readNat()
    (tag: @switch) match {
      case LITERALunit => Constant(())
      case LITERALboolean => Constant(readLong(len) != 0L)
      case LITERALbyte => Constant(readLong(len).toByte)
      case LITERALshort => Constant(readLong(len).toShort)
      case LITERALchar => Constant(readLong(len).toChar)
      case LITERALint => Constant(readLong(len).toInt)
      case LITERALlong => Constant(readLong(len))
      case LITERALfloat => Constant(intBitsToFloat(readLong(len).toInt))
      case LITERALdouble => Constant(longBitsToDouble(readLong(len)))
      case LITERALstring => Constant(readNameRef().toString)
      case LITERALnull => Constant(null)
      case LITERALclass => Constant(readTypeRef())
      case LITERALenum => Constant(readSymbolRef())
      case _ => noSuchConstantTag(tag, len)
    }
  }

  def noSuchConstantTag(tag: Int, len: Int)(implicit ctx: Context): Constant =
    errorBadSignature("bad constant tag: " + tag)

  /** Read children and store them into the corresponding symbol.
   */
  protected def readChildren()(implicit ctx: Context): Unit = {
    val tag = readByte()
    assert(tag == CHILDREN)
    val end = readNat() + readIndex
    val target = readSymbolRef()
    while (readIndex != end) {
      val start = readIndex
      readNat() // skip reference for now
      target.addAnnotation(
          Annotation.Child(implicit ctx =>
              atReadPos(start, () => readSymbolRef())))
    }
  }

  /* Read a reference to a pickled item */
  protected def readSymbolRef()(implicit ctx: Context): Symbol = { //OPT inlined from: at(readNat(), readSymbol) to save on closure creation
    val i = readNat()
    var r = entries(i)
    if (r eq null) {
      val savedIndex = readIndex
      readIndex = index(i)
      r = readSymbol()
      assert(entries(i) eq null, entries(i))
      entries(i) = r
      readIndex = savedIndex
    }
    r.asInstanceOf[Symbol]
  }

  protected def readDisambiguatedSymbolRef(p: Symbol => Boolean)(implicit ctx: Context): Symbol =
    at(readNat(), () => readDisambiguatedSymbol(p)())

  protected def readNameRef()(implicit ctx: Context): Name = at(readNat(), () => readName())
  protected def readTypeRef()(implicit ctx: Context): Type = at(readNat(), () => readType()) // after the NMT_TRANSITION period, we can leave off the () => ... ()
  protected def readConstantRef()(implicit ctx: Context): Constant = at(readNat(), () => readConstant())

  protected def readTypeNameRef()(implicit ctx: Context): TypeName = readNameRef().toTypeName
  protected def readTermNameRef()(implicit ctx: Context): TermName = readNameRef().toTermName

  protected def readAnnotationRef()(implicit ctx: Context): Annotation = at(readNat(), () => readAnnotation())

  protected def readModifiersRef(isType: Boolean)(implicit ctx: Context): Modifiers = at(readNat(), () => readModifiers(isType))
  protected def readTreeRef()(implicit ctx: Context): Tree = at(readNat(), () => readTree())

  /** Read an annotation argument, which is pickled either
   *  as a Constant or a Tree.
   */
  protected def readAnnotArg(i: Int)(implicit ctx: Context): Tree = bytes(index(i)) match {
    case TREE => at(i, () => readTree())
    case _ => Literal(at(i, () => readConstant()))
  }

  /** Read a ClassfileAnnotArg (argument to a classfile annotation)
   */
  private def readArrayAnnotArg()(implicit ctx: Context): Tree = {
    readByte() // skip the `annotargarray` tag
    val end = readNat() + readIndex
    // array elements are trees representing instances of scala.annotation.Annotation
    SeqLiteral(
      until(end, () => readClassfileAnnotArg(readNat())),
      TypeTree(defn.AnnotationType))
  }

  private def readAnnotInfoArg()(implicit ctx: Context): Tree = {
    readByte() // skip the `annotinfo` tag
    val end = readNat() + readIndex
    readAnnotationContents(end)
  }

  protected def readClassfileAnnotArg(i: Int)(implicit ctx: Context): Tree = bytes(index(i)) match {
    case ANNOTINFO => at(i, () => readAnnotInfoArg())
    case ANNOTARGARRAY => at(i, () => readArrayAnnotArg())
    case _ => readAnnotArg(i)
  }

  /** Read an annotation's contents. Not to be called directly, use
   *  readAnnotation, readSymbolAnnotation, or readAnnotInfoArg
   */
  protected def readAnnotationContents(end: Int)(implicit ctx: Context): Tree = {
    val atp = readTypeRef()
    val args = {
      val t = new ListBuffer[Tree]

      while (readIndex != end) {
        val argref = readNat()
        t += {
          if (isNameEntry(argref)) {
            val name = at(argref, () => readName())
            val arg = readClassfileAnnotArg(readNat())
            NamedArg(name.asTermName, arg)
          } else readAnnotArg(argref)
        }
      }
      t.toList
    }
    // println(atp)
    val targs = atp.argTypes

    tpd.applyOverloaded(tpd.New(atp.typeConstructor), nme.CONSTRUCTOR, args, targs, atp)
}

  /** Read an annotation and as a side effect store it into
   *  the symbol it requests. Called at top-level, for all
   *  (symbol, annotInfo) entries.
   */
  protected def readSymbolAnnotation()(implicit ctx: Context): Unit = {
    val tag = readByte()
    if (tag != SYMANNOT)
      errorBadSignature("symbol annotation expected (" + tag + ")")
    val end = readNat() + readIndex
    val target = readSymbolRef()
    target.addAnnotation(deferredAnnot(end))
  }

  /** Read an annotation and return it. Used when unpickling
   *  an ANNOTATED(WSELF)tpe or a NestedAnnotArg
   */
  protected def readAnnotation()(implicit ctx: Context): Annotation = {
    val tag = readByte()
    if (tag != ANNOTINFO)
      errorBadSignature("annotation expected (" + tag + ")")
    val end = readNat() + readIndex
    deferredAnnot(end)
  }

  /** A deferred annotation that can be completed by reading
   *  the bytes between `readIndex` and `end`.
   */
  protected def deferredAnnot(end: Int)(implicit ctx: Context): Annotation = {
    val start = readIndex
    val atp = readTypeRef()
    val phase = ctx.phase
    Annotation.deferred(
      atp.typeSymbol, implicit ctx =>
        atReadPos(start, () => readAnnotationContents(end)(ctx.withPhase(phase))))
  }

  /* Read an abstract syntax tree */
  protected def readTree()(implicit ctx: Context): Tree = {
    val outerTag = readByte()
    if (outerTag != TREE)
      errorBadSignature("tree expected (" + outerTag + ")")
    val end = readNat() + readIndex
    val tag = readByte()
    val tpe = if (tag == EMPTYtree) NoType else readTypeRef()

    // Set by the three functions to follow.  If symbol is non-null
    // after the new tree 't' has been created, t has its Symbol
    // set to symbol; and it always has its Type set to tpe.
    var symbol: Symbol = null
    var mods: Modifiers = null
    var name: Name = null

    /** Read a Symbol, Modifiers, and a Name */
    def setSymModsName(): Unit = {
      symbol = readSymbolRef()
      mods = readModifiersRef(symbol.isType)
      name = readNameRef()
    }
    /** Read a Symbol and a Name */
    def setSymName(): Unit = {
      symbol = readSymbolRef()
      name = readNameRef()
    }
    /** Read a Symbol */
    def setSym(): Unit = {
      symbol = readSymbolRef()
    }

    implicit val span: Span = NoSpan

    tag match {
      case EMPTYtree =>
        EmptyTree

      case PACKAGEtree =>
        setSym()
        val pid = readTreeRef().asInstanceOf[RefTree]
        val stats = until(end, () => readTreeRef())
        PackageDef(pid, stats)

      case CLASStree =>
        setSymModsName()
        val impl = readTemplateRef()
        val tparams = until(end, () => readTypeDefRef())
        val cls = symbol.asClass
        val ((constr: DefDef) :: Nil, stats) =
          impl.body.partition(_.symbol == cls.primaryConstructor)
        ClassDef(cls, constr, tparams ++ stats)

      case MODULEtree =>
        setSymModsName()
        ModuleDef(symbol.asTerm, readTemplateRef().body)

      case VALDEFtree =>
        setSymModsName()
        val tpt = readTreeRef()
        val rhs = readTreeRef()
        ValDef(symbol.asTerm, rhs)

      case DEFDEFtree =>
        setSymModsName()
        val tparams = times(readNat(), () => readTypeDefRef())
        val vparamss = times(readNat(), () => times(readNat(), () => readValDefRef()))
        val tpt = readTreeRef()
        val rhs = readTreeRef()
        DefDef(symbol.asTerm, rhs)

      case TYPEDEFtree =>
        setSymModsName()
        val rhs = readTreeRef()
        val tparams = until(end, () => readTypeDefRef())
        TypeDef(symbol.asType)

      case LABELtree =>
        ???
        setSymName()
        val rhs = readTreeRef()
        val params = until(end, () => readIdentRef())
        val ldef = DefDef(symbol.asTerm, rhs)
        def isCaseLabel(sym: Symbol) = sym.name.startsWith(nme.CASEkw.toString)
        if (isCaseLabel(symbol)) ldef
        else Block(ldef :: Nil, Apply(Ident(symbol.termRef), Nil))

      case IMPORTtree =>
        setSym()
        val expr = readTreeRef()
        val selectors = until(end, () => {
          val fromName = readNameRef()
          val toName = readNameRef()
          val from = untpd.Ident(fromName)
          val to = untpd.Ident(toName)
          if (toName.isEmpty) from else untpd.Thicket(from, untpd.Ident(toName))
        })
        Import(importImplied = false, expr, selectors)

      case TEMPLATEtree =>
        setSym()
        val parents = times(readNat(), () => readTreeRef())
        val self = readValDefRef()
        val body = until(end, () => readTreeRef())
        untpd.Template(???, parents, Nil, self, body) // !!! TODO: pull out primary constructor
          .withType(symbol.namedType)

      case BLOCKtree =>
        val expr = readTreeRef()
        val stats = until(end, () => readTreeRef())
        Block(stats, expr)

      case CASEtree =>
        val pat = readTreeRef()
        val guard = readTreeRef()
        val body = readTreeRef()
        CaseDef(pat, guard, body)

      case ALTERNATIVEtree =>
        Alternative(until(end, () => readTreeRef()))

      case STARtree =>
        readTreeRef()
        unimplementedTree("STAR")

      case BINDtree =>
        setSymName()
        Bind(symbol.asTerm, readTreeRef())

      case UNAPPLYtree =>
        val fun = readTreeRef()
        val args = until(end, () => readTreeRef())
        UnApply(fun, Nil, args, defn.AnyType) // !!! this is wrong in general

      case ARRAYVALUEtree =>
        val elemtpt = readTreeRef()
        val trees = until(end, () => readTreeRef())
        SeqLiteral(trees, elemtpt)
          // note can't deal with trees passed to Java methods as arrays here

      case FUNCTIONtree =>
        setSym()
        val body = readTreeRef()
        val vparams = until(end, () => readValDefRef())
        val applyType = MethodType(vparams map (_.name), vparams map (_.tpt.tpe), body.tpe)
        val applyMeth = ctx.newSymbol(symbol.owner, nme.apply, Method, applyType)
        Closure(applyMeth, Function.const(body.changeOwner(symbol, applyMeth)) _)

      case ASSIGNtree =>
        val lhs = readTreeRef()
        val rhs = readTreeRef()
        Assign(lhs, rhs)

      case IFtree =>
        val cond = readTreeRef()
        val thenp = readTreeRef()
        val elsep = readTreeRef()
        If(cond, thenp, elsep)

      case MATCHtree =>
        val selector = readTreeRef()
        val cases = until(end, () => readCaseDefRef())
        Match(selector, cases)

      case RETURNtree =>
        setSym()
        Return(readTreeRef(), Ident(symbol.termRef))

      case TREtree =>
        val block = readTreeRef()
        val finalizer = readTreeRef()
        val catches = until(end, () => readCaseDefRef())
        Try(block, catches, finalizer)

      case THROWtree =>
        Throw(readTreeRef())

      case NEWtree =>
        New(readTreeRef().tpe)

      case TYPEDtree =>
        val expr = readTreeRef()
        val tpt = readTreeRef()
        Typed(expr, tpt)

      case TYPEAPPLYtree =>
        val fun = readTreeRef()
        val args = until(end, () => readTreeRef())
        TypeApply(fun, args)

      case APPLYtree =>
        val fun = readTreeRef()
        val args = until(end, () => readTreeRef())
        /*
          if (fun.symbol.isOverloaded) {
            fun.setType(fun.symbol.info)
            inferMethodAlternative(fun, args map (_.tpe), tpe)
          }
*/
        Apply(fun, args) // note: can't deal with overloaded syms yet

      case APPLYDYNAMICtree =>
        setSym()
        val qual = readTreeRef()
        val args = until(end, () => readTreeRef())
        unimplementedTree("APPLYDYNAMIC")

      case SUPERtree =>
        setSym()
        val qual = readTreeRef()
        val mix = readTypeNameRef()
        Super(qual, mix, inConstrCall = false) // todo: revise

      case THIStree =>
        setSym()
        val name = readTypeNameRef()
        This(symbol.asClass)

      case SELECTtree =>
        setSym()
        val qualifier = readTreeRef()
        val selector = readNameRef()
        qualifier.select(symbol.namedType)
      case IDENTtree =>
        setSymName()
        Ident(symbol.namedType)

      case LITERALtree =>
        Literal(readConstantRef())

      case TYPEtree =>
        TypeTree(tpe)

      case ANNOTATEDtree =>
        val annot = readTreeRef()
        val arg = readTreeRef()
        Annotated(arg, annot)

      case SINGLETONTYPEtree =>
        SingletonTypeTree(readTreeRef())

      case SELECTFROMTYPEtree =>
        val qualifier = readTreeRef()
        val selector = readTypeNameRef()
        Select(qualifier, symbol.namedType)

      case COMPOUNDTYPEtree =>
        readTemplateRef()
        TypeTree(tpe)

      case APPLIEDTYPEtree =>
        val tpt = readTreeRef()
        val args = until(end, () => readTreeRef())
        AppliedTypeTree(tpt, args)

      case TYPEBOUNDStree =>
        val lo = readTreeRef()
        val hi = readTreeRef()
        TypeBoundsTree(lo, hi)

      case EXISTENTIALTYPEtree =>
        val tpt = readTreeRef()
        val whereClauses = until(end, () => readTreeRef())
        TypeTree(tpe)

      case _ =>
        noSuchTreeTag(tag, end)
    }
  }

  def noSuchTreeTag(tag: Int, end: Int)(implicit ctx: Context): Nothing =
    errorBadSignature("unknown tree type (" + tag + ")")

  def unimplementedTree(what: String)(implicit ctx: Context): Nothing =
    errorBadSignature(s"cannot read $what trees from Scala 2.x signatures")

  def readModifiers(isType: Boolean)(implicit ctx: Context): Modifiers = {
    val tag = readNat()
    if (tag != MODIFIERS)
      errorBadSignature("expected a modifiers tag (" + tag + ")")
    val end = readNat() + readIndex
    val pflagsHi = readNat()
    val pflagsLo = readNat()
    val pflags = (pflagsHi.toLong << 32) + pflagsLo
    val flags = unpickleScalaFlags(pflags, isType)
    val privateWithin = readNameRef().asTypeName
    Modifiers(flags, privateWithin, Nil)
  }

  protected def readTemplateRef()(implicit ctx: Context): Template =
    readTreeRef() match {
      case templ: Template => templ
      case other =>
        errorBadSignature("expected a template (" + other + ")")
    }
  protected def readCaseDefRef()(implicit ctx: Context): CaseDef =
    readTreeRef() match {
      case tree: CaseDef => tree
      case other =>
        errorBadSignature("expected a case def (" + other + ")")
    }
  protected def readValDefRef()(implicit ctx: Context): ValDef =
    readTreeRef() match {
      case tree: ValDef => tree
      case other =>
        errorBadSignature("expected a ValDef (" + other + ")")
    }
  protected def readIdentRef()(implicit ctx: Context): Ident =
    readTreeRef() match {
      case tree: Ident => tree
      case other =>
        errorBadSignature("expected an Ident (" + other + ")")
    }
  protected def readTypeDefRef()(implicit ctx: Context): TypeDef =
    readTreeRef() match {
      case tree: TypeDef => tree
      case other =>
        errorBadSignature("expected an TypeDef (" + other + ")")
    }

}
