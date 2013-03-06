package dotty.tools
package dotc
package core
package pickling

import java.io.IOException
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import Contexts._, Symbols._, Types._, Scopes._, SymDenotations._, Names._
import StdNames._, Denotations._, NameOps._, Flags._, Constants._, Annotations._
import Positions._, TypedTrees.tpd._, TypedTrees.TreeOps
import io.AbstractFile
import scala.reflect.internal.pickling.PickleFormat._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.annotation.switch

object UnPickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)

  case class TempPolyType(tparams: List[Symbol], tpe: Type) extends UncachedGroundType

  /** Temporary type for classinfos, will be decomposed on completion of the class */
  case class TempClassInfoType(parentTypes: List[Type], decls: MutableScope, clazz: Symbol) extends UncachedGroundType

  def depoly(tp: Type)(implicit ctx: Context): Type = tp match {
    case TempPolyType(tparams, restpe) => PolyType.fromSymbols(tparams, restpe)
    case tp => tp
  }

  /** Convert array parameters denoting a repeated parameter of a Java method
   *  to `JavaRepeatedParamClass` types.
   */
  def arrayToRepeated(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp @ MethodType(paramNames, paramTypes) =>
      val lastArg = paramTypes.last
      assert(lastArg.typeSymbol == defn.ArrayClass)
      val elemtp0 :: Nil = lastArg.typeArgs
      val elemtp = elemtp0 match {
        case AndType(t1, t2) if t1.typeSymbol.isAbstractType && t2.typeSymbol == defn.ObjectClass =>
          t1 // drop intersection with Object for abstract types in varargs. UnCurry can handle them.
        case _ =>
          elemtp0
      }
      tp.derivedMethodType(
          paramNames,
          paramTypes.init :+ defn.JavaRepeatedParamType.appliedTo(elemtp),
          tp.resultType)
    case tp @ PolyType(paramNames) =>
      tp.derivedPolyType(paramNames, tp.paramBounds, arrayToRepeated(tp.resultType))
  }

  def setClassInfo(denot: ClassDenotation, info: Type, optSelfType: Type = NoType)(implicit ctx: Context): Unit = {
    val cls = denot.classSymbol
    val (tparams, TempClassInfoType(parents, decls, clazz)) = info match {
      case TempPolyType(tps, cinfo) => (tps, cinfo)
      case cinfo => (Nil, cinfo)
    }
    val parentRefs = ctx.normalizeToRefs(parents, cls, decls)
    tparams foreach decls.enter
    denot.info = ClassInfo(denot.owner.thisType, denot.classSymbol, parentRefs, decls, optSelfType)
  }
}

/** Unpickle symbol table information descending from a class and/or module root
 *  from an array of bytes.
 *  @param bytes      bytearray from which we unpickle
 *  @param classroot  the top-level class which is unpickled, or NoSymbol if inapplicable
 *  @param moduleroot the top-level module class which is unpickled, or NoSymbol if inapplicable
 *  @param filename   filename associated with bytearray, only used for error messages
 */
class UnPickler(bytes: Array[Byte], classRoot: ClassDenotation, moduleRoot: ClassDenotation)(implicit cctx: CondensedContext)
  extends PickleBuffer(bytes, 0, -1) {

  import UnPickler._

  import cctx.debug

  checkVersion()

  private val loadingMirror = defn // was: mirrorThatLoaded(classRoot)

  /** A map from entry numbers to array offsets */
  private val index = createIndex

  /** A map from entry numbers to symbols, types, or annotations */
  private val entries = new Array[AnyRef](index.length)

  /** A map from symbols to their associated `decls` scopes */
  private val symScopes = mutable.HashMap[Symbol, Scope]()

  /** A map from refinement classes to their associated refinement types */
  private val refinementTypes = mutable.HashMap[Symbol, RefinedType]()

  protected def errorBadSignature(msg: String) = {
    val ex = new BadSignature(
      s"""error reading Scala signature of $classRoot from $source:
         |error occured at position $readIndex: $msg""".stripMargin)
    /*if (debug)*/ ex.printStackTrace()
    throw ex
  }

  // Laboriously unrolled for performance.
  def run() =
    try {
      var i = 0
      while (i < index.length) {
        if (entries(i) == null && isSymbolEntry(i)) {
          val savedIndex = readIndex
          readIndex = index(i)
          entries(i) = readSymbol()
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
      case ex: BadSignature =>
        throw ex
      case ex: RuntimeException =>
        errorBadSignature(s"a runtime exception occured: $ex $ex.getMessage()")
    }

  def source: AbstractFile = {
    val f = classRoot.symbol.associatedFile
    if (f != null) f else moduleRoot.symbol.associatedFile
  }

  private def checkVersion() {
    val major = readNat()
    val minor = readNat()
    if (major != MajorVersion || minor > MinorVersion)
      throw new IOException("Scala signature " + classRoot.fullName.decode +
        " has wrong version\n expected: " +
        MajorVersion + "." + MinorVersion +
        "\n found: " + major + "." + minor +
        " in " + source)
  }

  /** Pickle = majorVersion_Nat minorVersion_Nat nbEntries_Nat {Entry}
   *  Entry  = type_Nat length_Nat [actual entries]
   *
   *  Assumes that the ..Version_Nat are already consumed.
   *
   *  @return an array mapping entry numbers to locations in
   *  the byte array where the entries start.
   */
  def createIndex: Array[Int] = {
    val index = new Array[Int](readNat()) // nbEntries_Nat
    for (i <- 0 until index.length) {
      index(i) = readIndex
      readByte() // skip type_Nat
      readIndex = readNat() + readIndex // read length_Nat, jump to next entry
    }
    index
  }

  /** The `decls` scope associated with given symbol */
  protected def symScope(sym: Symbol) = symScopes.getOrElseUpdate(sym, newScope)

  /** Does entry represent an (internal) symbol */
  protected def isSymbolEntry(i: Int): Boolean = {
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
  protected def isRefinementSymbolEntry(i: Int): Boolean = {
    val savedIndex = readIndex
    readIndex = index(i)
    val tag = readByte().toInt
    assert(tag == CLASSsym)

    readNat(); // read length
    val result = readNameRef() == tpnme.REFINE_CLASS
    readIndex = savedIndex
    result
  }

  protected def isRefinementClass(sym: Symbol): Boolean =
    sym.name == tpnme.REFINE_CLASS

  protected def isLocal(sym: Symbol) = isUnpickleRoot(sym.topLevelClass)

  protected def isUnpickleRoot(sym: Symbol) = {
    val d = sym.denot
    d == moduleRoot || d == classRoot
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
  protected def readName(): Name = {
    val tag = readByte()
    val len = readNat()
    tag match {
      case TERMname => termName(bytes, readIndex, len)
      case TYPEname => typeName(bytes, readIndex, len)
      case _ => errorBadSignature("bad name tag: " + tag)
    }
  }
  protected def readTermName(): TermName = readName().toTermName
  protected def readTypeName(): TypeName = readName().toTypeName

  /** Read a symbol */
  protected def readSymbol(): Symbol = readDisambiguatedSymbol(scala.Function.const(true))()

  /** Read a symbol, with possible disambiguation */
  protected def readDisambiguatedSymbol(p: Symbol => Boolean)(): Symbol = {
    val start = indexCoord(readIndex)
    val tag = readByte()
    val end = readNat() + readIndex
    def atEnd = readIndex == end

    def readExtSymbol(): Symbol = {
      val name = readNameRef()
      val owner = if (atEnd) loadingMirror.RootClass else readSymbolRef()

      def adjust(denot: Denotation) = {
        val denot1 = denot.disambiguate(p)
        val sym = denot1.symbol
        if (tag == EXTref) sym else sym.moduleClass
      }

      def fromName(name: Name): Symbol = name.toTermName match {
        case nme.ROOT => loadingMirror.RootClass
        case nme.ROOTPKG => loadingMirror.RootPackage
        case _ => adjust(owner.info.decl(name))
      }

      def nestedObjectSymbol: Symbol = {
        // If the owner is overloaded (i.e. a method), it's not possible to select the
        // right member, so return NoSymbol. This can only happen when unpickling a tree.
        // the "case Apply" in readTree() takes care of selecting the correct alternative
        //  after parsing the arguments.
        //if (owner.isOverloaded)
        //  return NoSymbol

        if (tag == EXTMODCLASSref) {
          unimplementedTree(s"nested objects")
          /*
            val moduleVar = owner.info.decl(name.toTermName.moduleVarName).symbol
            if (moduleVar.isLazyAccessor)
              return moduleVar.lazyAccessor.lazyAccessor
*/
        }
        NoSymbol
      }

      // (1) Try name.
      fromName(name) orElse {
        // (2) Try with expanded name.  Can happen if references to private
        // symbols are read from outside: for instance when checking the children
        // of a class.  See #1722.
        fromName(name.toTermName.expandedName(owner)) orElse {
          // (3) Try as a nested object symbol.
          nestedObjectSymbol orElse {
            //              // (4) Call the mirror's "missing" hook.
            adjust(cctx.base.missingHook(owner, name)) orElse {
              //              }
              // (5) Create a stub symbol to defer hard failure a little longer.
              cctx.newStubSymbol(owner, name, source)
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
    val name = at(nameref, readName)
    val owner = readSymbolRef()
    val flags = unpickleScalaFlags(readLongNat(), name.isTypeName)

    def isClassRoot = (name == classRoot.name) && (owner == classRoot.owner)
    def isModuleRoot = (name.toTermName == moduleRoot.name.toTermName) && (owner == moduleRoot.owner)

    def completeRoot(denot: ClassDenotation): Symbol = {
      atReadPos(start.toIndex, () => localUnpickler.parseToCompletion(denot))
      denot.symbol
    }

    def finishSym(sym: Symbol): Symbol = {
      if (sym.owner.isClass && !(
        isUnpickleRoot(sym) ||
        (sym is (ModuleClass | TypeParam | Scala2Existential)) ||
        isRefinementClass(sym)))
        symScope(sym.owner).openForMutations.enter(sym)
      sym
    }

    finishSym(tag match {
      case TYPEsym | ALIASsym =>
        var name1 = name.asTypeName
        var flags1 = flags
        if (flags is TypeParam) {
          name1 = name1.expandedName(owner)
          flags1 |= TypeParamCreationFlags
        }
        cctx.newSymbol(owner, name1, flags1, localUnpickler, coord = start)
      case CLASSsym =>
        if (isClassRoot) completeRoot(classRoot)
        else if (isModuleRoot) completeRoot(moduleRoot)
        else cctx.newClassSymbol(owner, name.asTypeName, flags, localUnpickler, coord = start)
      case MODULEsym | VALsym =>
        if (isModuleRoot) {
          moduleRoot.flags = flags
          moduleRoot.symbol
        } else cctx.newSymbol(owner, name.asTermName, flags, localUnpickler, coord = start)
      case _ =>
        errorBadSignature("bad symbol tag: " + tag)
    })
  }

  val localUnpickler = new LazyType {
    def parseToCompletion(denot: SymDenotation) = {
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
          val pw = at(inforef, readSymbol)
          inforef = readNat()
          pw
        }
      val tp = at(inforef, () => readType(forceProperType = denot.isTerm))
      denot match {
        case denot: ClassDenotation =>
          val optSelfType = if (atEnd) NoType else readTypeRef()
          setClassInfo(denot, tp, optSelfType)
        case denot =>
          denot.info = if (tag == ALIASsym) TypeAlias(tp) else depoly(tp)
          if (atEnd) {
            assert(!(denot is SuperAccessor), denot)
          } else {
            assert(denot is (SuperAccessor | ParamAccessor), denot)
            def disambiguate(alt: Symbol) =
              denot.info =:= denot.owner.thisType.memberInfo(alt)
            val aliasRef = readNat()
            val alias = at(aliasRef, readDisambiguatedSymbol(disambiguate)).asTerm
            denot.addAnnotation(Annotation.makeAlias(alias))
          }
      }
    }
    def complete(denot: SymDenotation): Unit = {
      atReadPos(denot.symbol.coord.toIndex, () => parseToCompletion(denot))
    }
  }

  /** Convert
   *    tp { type name = sym } forSome { sym >: L <: H }
   *  to
   *    tp { name >: L <: H }
   *  and
   *    tp { name: sym } forSome { sym <: T with Singleton }
   *  to
   *    tp { name: T }
   */
  def elimExistentials(boundSyms: List[Symbol], tp: Type): Type = {
    def removeSingleton(tp: Type): Type =
      if (tp.typeSymbol == defn.SingletonClass) defn.AnyType else tp
    def elim(tp: Type): Type = tp match {
      case tp @ RefinedType(parent, name) =>
        val parent1 = elim(tp.parent)
        tp.refinedInfo match {
          case TypeAlias(info: TypeRefBySym) if boundSyms contains info.fixedSym =>
            RefinedType(parent1, name, info.fixedSym.info)
          case info: TypeRefBySym if boundSyms contains info.fixedSym =>
            val info1 = info.fixedSym.info
            assert(info1 <:< defn.SingletonClass.typeConstructor)
            RefinedType(parent1, name, info1.mapAnd(removeSingleton))
          case info =>
            tp.derivedRefinedType(parent1, name, info)
        }
      case _ =>
        tp
    }
    val tp1 = elim(tp)
    val isBound = (tp: Type) => boundSyms contains tp.typeSymbol
    if (tp1 exists isBound) {
      val tp2 = tp1.subst(boundSyms, boundSyms map (_ => defn.AnyType))
      cctx.warning(s"""failure to eliminate existential
                       |original type    : $tp forSome {${cctx.show(boundSyms, "; ")}}
                       |reduces to       : $tp1
                       |type used instead: $tp2
                       |proceed at own risk.""".stripMargin)
      tp2
    } else tp1
  }

  /** Read a type
   *
   *  @param forceProperType is used to ease the transition to NullaryMethodTypes (commentmarker: NMT_TRANSITION)
   *        the flag say that a type of kind * is expected, so that PolyType(tps, restpe) can be disambiguated to PolyType(tps, NullaryMethodType(restpe))
   *        (if restpe is not a ClassInfoType, a MethodType or a NullaryMethodType, which leaves TypeRef/SingletonType -- the latter would make the polytype a type constructor)
   */
  protected def readType(forceProperType: Boolean = false): Type = {
    val tag = readByte()
    val end = readNat() + readIndex
    (tag: @switch) match {
      case NOtpe =>
        NoType
      case NOPREFIXtpe =>
        NoPrefix
      case THIStpe =>
        val cls = readSymbolRef().asClass
        if (isRefinementClass(cls)) RefinedThis(refinementTypes(cls))
        else ThisType(cls)
      case SINGLEtpe =>
        val pre = readTypeRef()
        val sym = readDisambiguatedSymbol(_.isParameterless)
        if (isLocal(sym)) TermRef(pre, sym.asTerm)
        else TermRef(pre, sym.name.asTermName, NotAMethod)
      case SUPERtpe =>
        val thistpe = readTypeRef()
        val supertpe = readTypeRef()
        SuperType(thistpe, supertpe)
      case CONSTANTtpe =>
        ConstantType(readConstantRef())
      case TYPEREFtpe =>
        val pre = readTypeRef()
        val sym = readSymbolRef()
        val tycon =
          if (isLocal(sym)) TypeRef(pre, sym.asType)
          else TypeRef(pre, sym.name.asTypeName)
        tycon.appliedTo(until(end, readTypeRef))
      case TYPEBOUNDStpe =>
        TypeBounds(readTypeRef(), readTypeRef())
      case REFINEDtpe =>
        val clazz = readSymbolRef()
        val decls = symScope(clazz)
        symScopes(clazz) = EmptyScope // prevent further additions
        val parents = until(end, readTypeRef)
        val parent = parents.reduceLeft(_ & _)
        if (decls.isEmpty) parent
        else {
          def addRefinement(tp: Type, sym: Symbol) =
            RefinedType(tp, sym.name, sym.info)
          val result = (parent /: decls.toList)(addRefinement).asInstanceOf[RefinedType]
          assert(!refinementTypes.isDefinedAt(clazz), clazz + "/" + decls)
          refinementTypes(clazz) = result
          result
        }
      case CLASSINFOtpe =>
        val clazz = readSymbolRef()
        TempClassInfoType(until(end, readTypeRef), symScope(clazz).openForMutations, clazz)
      case METHODtpe | IMPLICITMETHODtpe =>
        val restpe = readTypeRef()
        val params = until(end, readSymbolRef)
        val maker = if (tag == METHODtpe) MethodType else ImplicitMethodType
        maker.fromSymbols(params, restpe)
      case POLYtpe =>
        val restpe = readTypeRef()
        val typeParams = until(end, readSymbolRef)
        if (typeParams.nonEmpty) {
          // NMT_TRANSITION: old class files denoted a polymorphic nullary method as PolyType(tps, restpe), we now require PolyType(tps, NullaryMethodType(restpe))
          // when a type of kind * is expected (forceProperType is true), we know restpe should be wrapped in a NullaryMethodType (if it wasn't suitably wrapped yet)
          def transitionNMT(restpe: Type) = {
            val resTpeCls = restpe.getClass.toString // what's uglier than isInstanceOf? right! -- isInstanceOf does not work since the concrete types are defined in the compiler (not in scope here)
            if (forceProperType /*&& pickleformat < 2.9 */ && !(resTpeCls.endsWith("MethodType"))) {
              assert(!resTpeCls.contains("ClassInfoType"))
              ExprType(restpe)
            } else restpe
          }
          TempPolyType(typeParams, transitionNMT(restpe))
        } else ExprType(restpe)
      case EXISTENTIALtpe =>
        val restpe = readTypeRef()
        val boundSyms = until(end, readSymbolRef)
        elimExistentials(boundSyms, restpe)
      case ANNOTATEDtpe =>
        val tp = readTypeRef()
        // no annotation self type is supported, so no test whether this is a symbol ref
        val annots = until(end, readAnnotationRef)
        AnnotatedType.make(annots, tp)
      case _ =>
        noSuchTypeTag(tag, end)
    }
  }

  def noSuchTypeTag(tag: Int, end: Int): Type =
    errorBadSignature("bad type tag: " + tag)

  /** Read a constant */
  protected def readConstant(): Constant = {
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

  def noSuchConstantTag(tag: Int, len: Int): Constant =
    errorBadSignature("bad constant tag: " + tag)

  /** Read children and store them into the corresponding symbol.
   */
  protected def readChildren() {
    val tag = readByte()
    assert(tag == CHILDREN)
    val end = readNat() + readIndex
    val target = readSymbolRef()
    while (readIndex != end)
      target.addAnnotation(Annotation.makeChild(readSymbolRef()))
  }

  /* Read a reference to a pickled item */
  protected def readSymbolRef(): Symbol = { //OPT inlined from: at(readNat(), readSymbol) to save on closure creation
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

  protected def readNameRef(): Name = at(readNat(), readName)
  protected def readTypeRef(): Type = at(readNat(), () => readType()) // after the NMT_TRANSITION period, we can leave off the () => ... ()
  protected def readConstantRef(): Constant = at(readNat(), readConstant)

  protected def readTypeNameRef(): TypeName = readNameRef().toTypeName
  protected def readTermNameRef(): TermName = readNameRef().toTermName

  protected def readAnnotationRef(): Annotation = at(readNat(), readAnnotation)

  protected def readModifiersRef(isType: Boolean): Modifiers = at(readNat(), () => readModifiers(isType))
  protected def readTreeRef(): Tree = at(readNat(), readTree)

  /** Read an annotation argument, which is pickled either
   *  as a Constant or a Tree.
   */
  protected def readAnnotArg(i: Int): Tree = bytes(index(i)) match {
    case TREE => at(i, readTree)
    case _ => Literal(at(i, readConstant))
  }

  /** Read a ClassfileAnnotArg (argument to a classfile annotation)
   */
  private def readArrayAnnotArg(): Tree = {
    readByte() // skip the `annotargarray` tag
    val end = readNat() + readIndex
    // array elements are trees representing instances of scala.annotation.Annotation
    SeqLiteral(
      TypeTree(defn.AnnotationClass.typeConstructor),
      until(end, () => readClassfileAnnotArg(readNat())))
  }

  private def readAnnotInfoArg(): Tree = {
    readByte() // skip the `annotinfo` tag
    val end = readNat() + readIndex
    readAnnotationContents(end)
  }

  protected def readClassfileAnnotArg(i: Int): Tree = bytes(index(i)) match {
    case ANNOTINFO => at(i, readAnnotInfoArg)
    case ANNOTARGARRAY => at(i, readArrayAnnotArg)
    case _ => readAnnotArg(i)
  }

  /** Read an annotation's contents. Not to be called directly, use
   *  readAnnotation, readSymbolAnnotation, or readAnnotInfoArg
   */
  protected def readAnnotationContents(end: Int): Tree = {
    val atp = readTypeRef()
    val args = new ListBuffer[Tree]
    while (readIndex != end) {
      val argref = readNat()
      args += {
        if (isNameEntry(argref)) {
          val name = at(argref, readName)
          val arg = readClassfileAnnotArg(readNat())
          NamedArg(name.asTermName, arg)
        } else readAnnotArg(argref)
      }
    }
    New(atp, args.toList)
  }

  /** Read an annotation and as a side effect store it into
   *  the symbol it requests. Called at top-level, for all
   *  (symbol, annotInfo) entries.
   */
  protected def readSymbolAnnotation(): Unit = {
    val tag = readByte()
    if (tag != SYMANNOT)
      errorBadSignature("symbol annotation expected (" + tag + ")")
    val end = readNat() + readIndex
    val target = readSymbolRef()
    target.addAnnotation(ConcreteAnnotation(readAnnotationContents(end)))
  }

  /** Read an annotation and return it. Used when unpickling
   *  an ANNOTATED(WSELF)tpe or a NestedAnnotArg
   */
  protected def readAnnotation(): Annotation = {
    val tag = readByte()
    if (tag != ANNOTINFO)
      errorBadSignature("annotation expected (" + tag + ")")
    val end = readNat() + readIndex
    ConcreteAnnotation(readAnnotationContents(end))
  }

  /* Read an abstract syntax tree */
  protected def readTree(): Tree = {
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
    def setSymModsName() {
      symbol = readSymbolRef()
      mods = readModifiersRef(symbol.isType)
      name = readNameRef()
    }
    /** Read a Symbol and a Name */
    def setSymName() {
      symbol = readSymbolRef()
      name = readNameRef()
    }
    /** Read a Symbol */
    def setSym() {
      symbol = readSymbolRef()
    }

    implicit val pos: Position = NoPosition

    tag match {
      case EMPTYtree =>
        EmptyTree

      case PACKAGEtree =>
        setSym()
        val pid = readTreeRef().asInstanceOf[RefTree]
        val stats = until(end, readTreeRef)
        PackageDef(pid, stats)

      case CLASStree =>
        setSymModsName()
        val impl = readTemplateRef()
        val tparams = until(end, readTypeDefRef)
        ClassDef(symbol.asClass, tparams map (_.symbol.asType), impl.body)

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
        val tparams = times(readNat(), readTypeDefRef)
        val vparamss = times(readNat(), () => times(readNat(), readValDefRef))
        val tpt = readTreeRef()
        val rhs = readTreeRef()
        DefDef(symbol.asTerm, rhs)

      case TYPEDEFtree =>
        setSymModsName()
        val rhs = readTreeRef()
        val tparams = until(end, readTypeDefRef)
        TypeDef(symbol.asType)

      case LABELtree =>
        setSymName()
        val rhs = readTreeRef()
        val params = until(end, readIdentRef)
        val ldef = DefDef(symbol.asTerm, rhs)
        def isCaseLabel(sym: Symbol) = sym.name.startsWith(nme.CASEkw)
        if (isCaseLabel(symbol)) ldef
        else Block(ldef :: Nil, Apply(Ident(refType(symbol)), Nil))

      case IMPORTtree =>
        setSym()
        val expr = readTreeRef()
        val selectors = until(end, () => {
          val fromName = readNameRef()
          val toName = readNameRef()
          val from = Trees.Ident(fromName)
          val to = Trees.Ident(toName)
          if (toName.isEmpty) from else Trees.Pair[Nothing](from, Trees.Ident(toName))
        })

        Import(expr, selectors)

      case TEMPLATEtree =>
        setSym()
        val parents = times(readNat(), readTreeRef)
        val self = readValDefRef()
        val body = until(end, readTreeRef)
        Trees.Template[Type](parents, self, body)
          .withType(refType(symbol))

      case BLOCKtree =>
        val expr = readTreeRef()
        val stats = until(end, readTreeRef)
        Block(stats, expr)

      case CASEtree =>
        val pat = readTreeRef()
        val guard = readTreeRef()
        val body = readTreeRef()
        CaseDef(pat, guard, body)

      case ALTERNATIVEtree =>
        Alternative(until(end, readTreeRef))

      case STARtree =>
        readTreeRef()
        unimplementedTree("STAR")

      case BINDtree =>
        setSymName()
        Bind(symbol.asTerm, readTreeRef())

      case UNAPPLYtree =>
        val fun = readTreeRef()
        val args = until(end, readTreeRef)
        UnApply(fun, args)

      case ARRAYVALUEtree =>
        val elemtpt = readTreeRef()
        val trees = until(end, readTreeRef)
        SeqLiteral(elemtpt, trees)

      case FUNCTIONtree =>
        setSym()
        val body = readTreeRef()
        val vparams = until(end, readValDefRef)
        val applyType = MethodType(vparams map (_.name), vparams map (_.tpt.tpe), body.tpe)
        val applyMeth = cctx.newSymbol(symbol.owner, nme.apply, Method, applyType)
        Function(applyMeth, body.changeOwner(symbol, applyMeth), tpe)

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
        val cases = until(end, readCaseDefRef)
        Match(selector, cases)

      case RETURNtree =>
        setSym()
        Return(readTreeRef(), Ident(refType(symbol)))

      case TREtree =>
        val block = readTreeRef()
        val finalizer = readTreeRef()
        val catches = until(end, readCaseDefRef)
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
        val args = until(end, readTreeRef)
        TypeApply(fun, args)

      case APPLYtree =>
        val fun = readTreeRef()
        val args = until(end, readTreeRef)
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
        val args = until(end, readTreeRef)
        unimplementedTree("APPLYDYNAMIC")

      case SUPERtree =>
        setSym()
        val qual = readTreeRef()
        val mix = readTypeNameRef()
        Super(qual, mix)

      case THIStree =>
        setSym()
        val name = readTypeNameRef()
        This(symbol.asClass)

      case SELECTtree =>
        setSym()
        val qualifier = readTreeRef()
        val selector = readNameRef()
        Select(qualifier, refType(symbol))

      case IDENTtree =>
        setSymName()
        Ident(refType(symbol))

      case LITERALtree =>
        Literal(readConstantRef())

      case TYPEtree =>
        TypeTree(tpe)

      case ANNOTATEDtree =>
        val annot = readTreeRef()
        val arg = readTreeRef()
        Annotated(annot, arg)

      case SINGLETONTYPEtree =>
        SingletonTypeTree(readTreeRef())

      case SELECTFROMTYPEtree =>
        val qualifier = readTreeRef()
        val selector = readTypeNameRef()
        SelectFromTypeTree(qualifier, refType(symbol))

      case COMPOUNDTYPEtree =>
        readTemplateRef()
        TypeTree(tpe)

      case APPLIEDTYPEtree =>
        val tpt = readTreeRef()
        val args = until(end, readTreeRef)
        AppliedTypeTree(tpt, args)

      case TYPEBOUNDStree =>
        val lo = readTreeRef()
        val hi = readTreeRef()
        TypeBoundsTree(lo, hi)

      case EXISTENTIALTYPEtree =>
        val tpt = readTreeRef()
        val whereClauses = until(end, readTreeRef)
        TypeTree(tpe)

      case _ =>
        noSuchTreeTag(tag, end)
    }
  }

  def noSuchTreeTag(tag: Int, end: Int) =
    errorBadSignature("unknown tree type (" + tag + ")")

  def unimplementedTree(what: String) =
    errorBadSignature(s"cannot read $what trees from Scala 2.x signatures")

  def readModifiers(isType: Boolean): Modifiers = {
    val tag = readNat()
    if (tag != MODIFIERS)
      errorBadSignature("expected a modifiers tag (" + tag + ")")
    val end = readNat() + readIndex
    val pflagsHi = readNat()
    val pflagsLo = readNat()
    val pflags = (pflagsHi.toLong << 32) + pflagsLo
    val flags = unpickleScalaFlags(pflags, isType)
    val privateWithin = readNameRef().asTypeName
    Trees.Modifiers[Type](flags, privateWithin, Nil)
  }

  protected def readTemplateRef(): Template =
    readTreeRef() match {
      case templ: Template => templ
      case other =>
        errorBadSignature("expected a template (" + other + ")")
    }
  protected def readCaseDefRef(): CaseDef =
    readTreeRef() match {
      case tree: CaseDef => tree
      case other =>
        errorBadSignature("expected a case def (" + other + ")")
    }
  protected def readValDefRef(): ValDef =
    readTreeRef() match {
      case tree: ValDef => tree
      case other =>
        errorBadSignature("expected a ValDef (" + other + ")")
    }
  protected def readIdentRef(): Ident =
    readTreeRef() match {
      case tree: Ident => tree
      case other =>
        errorBadSignature("expected an Ident (" + other + ")")
    }
  protected def readTypeDefRef(): TypeDef =
    readTreeRef() match {
      case tree: TypeDef => tree
      case other =>
        errorBadSignature("expected an TypeDef (" + other + ")")
    }

}
