package dotty.tools
package dotc
package core
package tasty

import scala.language.unsafeNulls

import Comments.docCtx
import Contexts.*
import Symbols.*
import Types.*
import Scopes.*
import SymDenotations.*
import Denotations.*
import Names.*
import NameOps.*
import StdNames.*
import Flags.*
import Constants.*
import Annotations.*
import NameKinds.*
import NamerOps.*
import ContextOps.*
import Variances.Invariant
import TastyUnpickler.NameTable
import typer.ConstFold
import typer.Checking.checkNonCyclic
import typer.Nullables.*
import util.Spans.*
import util.{SourceFile, Property}
import ast.{Trees, tpd, untpd}
import Trees.*
import Decorators.*
import dotty.tools.dotc.quoted.QuotePatterns

import dotty.tools.tasty.{TastyBuffer, TastyReader}
import TastyBuffer.*

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import config.Printers.pickling

import dotty.tools.tasty.TastyFormat.*
import dotty.tools.tasty.besteffort.BestEffortTastyFormat.ERRORtype

import scala.annotation.constructorOnly
import scala.annotation.internal.sharable
import scala.compiletime.uninitialized

/** Unpickler for typed trees
 *  @param reader              the reader from which to unpickle
 *  @param compilationUnitInfo  the compilation unit info of the TASTy
 *  @param posUnpicklerOpt     the unpickler for positions, if it exists
 *  @param commentUnpicklerOpt the unpickler for comments, if it exists
 *  @param attributeUnpicklerOpt the unpickler for attributes, if it exists
 *  @param isBestEffortTasty   decides whether to unpickle as a Best Effort TASTy
 */
class TreeUnpickler(reader: TastyReader,
                    nameAtRef: NameTable,
                    compilationUnitInfo: CompilationUnitInfo,
                    posUnpicklerOpt: Option[PositionUnpickler],
                    commentUnpicklerOpt: Option[CommentUnpickler],
                    isBestEffortTasty: Boolean = false) {
  import TreeUnpickler.*
  import tpd.*

  /** A map from addresses of definition entries to the symbols they define */
  private val symAtAddr  = new mutable.HashMap[Addr, Symbol]

  private def addrOfSymbol(sym: Symbol): Option[Addr] = symAtAddr.iterator.collectFirst {
    case (addr, s) if s == sym => addr
  }

  private def locatedSymbol(sym: Symbol)(using Context): String =
    addrOfSymbol(sym) match
      case Some(addr) => i"local $sym @ ${addr.index}"
      case None => i"external $sym"

  /** A temporary map from addresses of definition entries to the trees they define.
   *  Used to remember trees of symbols that are created by a completion. Emptied
   *  once the tree is inlined into a larger tree.
   */
  private val treeAtAddr = new mutable.HashMap[Addr, Tree]

  /** A map from addresses of type entries to the types they define.
   *  Currently only populated for types that might be recursively referenced
   *  from within themselves (i.e. RecTypes, LambdaTypes).
   */
  private val typeAtAddr = new mutable.HashMap[Addr, Type]

  /** If this is a pickled quote, the owner of the quote, otherwise NoSymbol. */
  private var rootOwner: Symbol = NoSymbol

  /** The root symbol denotation which are defined by the Tasty file associated with this
   *  TreeUnpickler. Set by `enterTopLevel`.
   */
  private var roots: Set[SymDenotation] = null

  /** The root symbols that are defined in this Tasty file. This
   *  is a subset of `roots.map(_.symbol)`.
   */
  private var seenRoots: Set[Symbol] = Set()

  /** The root owner tree. See `OwnerTree` class definition. Set by `enterTopLevel`. */
  private var ownerTree: OwnerTree = uninitialized

  /** TASTy attributes */
  private val attributes: Attributes = compilationUnitInfo.tastyInfo.get.attributes

  /** Was unpickled class compiled with capture checks? */
  private val withCaptureChecks: Boolean = attributes.captureChecked

  private val unpicklingScala2Library = attributes.scala2StandardLibrary

  /** This dependency was compiled with explicit nulls enabled */
  // TODO Use this to tag the symbols of this dependency as compiled with explicit nulls (see use of unpicklingScala2Library).
  private val explicitNulls = attributes.explicitNulls

  private val unpicklingJava = attributes.isJava

  private val isOutline = attributes.isOutline

  private def registerSym(addr: Addr, sym: Symbol) =
    symAtAddr(addr) = sym

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enter(roots: Set[SymDenotation])(using Context): Unit = {
    this.roots = roots
    val rdr = new TreeReader(reader).fork
    ownerTree = new OwnerTree(NoAddr, 0, rdr.fork, reader.endAddr)
    if (rdr.isTopLevel)
      rdr.indexStats(reader.endAddr)
  }

  /** The unpickled trees */
  def unpickle(mode: UnpickleMode)(using Context): List[Tree] = {
    if mode != UnpickleMode.TopLevel then rootOwner = ctx.owner
    assert(roots != null, "unpickle without previous enterTopLevel")
    val rdr = new TreeReader(reader)
    mode match {
      case UnpickleMode.TopLevel => rdr.readTopLevel()
      case UnpickleMode.Term => rdr.readTree() :: Nil
      case UnpickleMode.TypeTree => rdr.readTpt() :: Nil
    }
  }

  class Completer(reader: TastyReader)(using @constructorOnly _ctx: Context) extends LazyType {
    import reader.*
    val owner = ctx.owner
    val mode = ctx.mode
    val source = ctx.source
    def complete(denot: SymDenotation)(using Context): Unit =
      def where =
        val f = denot.symbol.associatedFile
        if f == null then "" else s" in $f"
      def fail(ex: Throwable) = throw UnpicklingError(denot, where, ex)
      treeAtAddr(currentAddr) =
        val traceCycles = CyclicReference.isTraced
        try
          if traceCycles then
            CyclicReference.pushTrace("read the definition of ", denot.symbol, where)
          atPhaseBeforeTransforms {
            new TreeReader(reader).readIndexedDef()(
              using ctx.withOwner(owner).withModeBits(mode).withSource(source))
          }
        catch
          case ex: CyclicReference => throw ex
          case ex: AssertionError => fail(ex)
          case ex: Exception => fail(ex)
        finally
          if traceCycles then CyclicReference.popTrace()
  }

  class TreeReader(val reader: TastyReader) {
    import reader.*

    def forkAt(start: Addr): TreeReader = new TreeReader(subReader(start, endAddr))
    def fork: TreeReader = forkAt(currentAddr)

    def skipParentTree(tag: Int): Unit = {
      if tag == SPLITCLAUSE then ()
      else skipTree(tag)
    }
    def skipParentTree(): Unit = skipParentTree(readByte())
    def skipTree(tag: Int): Unit = {
      if (tag >= firstLengthTreeTag) goto(readEnd())
      else if (tag >= firstNatASTTreeTag) { readNat(); skipTree() }
      else if (tag >= firstASTTreeTag) skipTree()
      else if (tag >= firstNatTreeTag) readNat()
    }
    def skipTree(): Unit = skipTree(readByte())

    def skipParams(): Unit =
      while
        val tag = nextByte
        tag == PARAM || tag == TYPEPARAM || tag == EMPTYCLAUSE || tag == SPLITCLAUSE
      do skipTree()

    /** Record all directly nested definitions and templates in current tree
     *  as `OwnerTree`s in `buf`.
     *  A complication concerns member definitions. These are lexically nested in a
     *  Template node, but need to be listed separately in the OwnerTree of the enclosing class
     *  in order not to confuse owner chains.
     */
    def scanTree(buf: ListBuffer[OwnerTree], mode: MemberDefMode = AllDefs): Unit = {
      val start = currentAddr
      val tag = readByte()
      tag match {
        case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | TEMPLATE =>
          val end = readEnd()
          for (i <- 0 until numRefs(tag)) readNat()
          if (tag == TEMPLATE) {
            // Read all member definitions now, whereas non-members are children of
            // template's owner tree.
            val nonMemberReader = fork
            scanTrees(buf, end, MemberDefsOnly)
            buf += new OwnerTree(start, tag, nonMemberReader, end)
          }
          else if (mode != NoMemberDefs)
            buf += new OwnerTree(start, tag, fork, end)
          goto(end)
        case tag =>
          if (mode == MemberDefsOnly) skipTree(tag)
          else if (tag >= firstLengthTreeTag) {
            val end = readEnd()
            var nrefs = numRefs(tag)
            if (nrefs < 0) {
              for (i <- nrefs until 0) scanTree(buf)
              goto(end)
            }
            else {
              for (i <- 0 until nrefs) readNat()
              if (tag == BIND)
                // a Bind is never the owner of anything, so we set `end = start`
                buf += new OwnerTree(start, tag, fork, end = start)

              scanTrees(buf, end)
            }
          }
          else if (tag >= firstNatASTTreeTag) { readNat(); scanTree(buf) }
          else if (tag >= firstASTTreeTag) scanTree(buf)
          else if (tag >= firstNatTreeTag) readNat()
      }
    }

    /** Record all directly nested definitions and templates between current address and `end`
     *  as `OwnerTree`s in `buf`
     */
    def scanTrees(buf: ListBuffer[OwnerTree], end: Addr, mode: MemberDefMode = AllDefs): Unit = {
      while (currentAddr.index < end.index) scanTree(buf, mode)
      assert(currentAddr.index == end.index)
    }

    /** The next tag, following through SHARED tags */
    def nextUnsharedTag: Int = {
      val tag = nextByte
      if (tag == SHAREDtype || tag == SHAREDterm) {
        val lookAhead = fork
        lookAhead.reader.readByte()
        forkAt(lookAhead.reader.readAddr()).nextUnsharedTag
      }
      else tag
    }

    def readName(): TermName = nameAtRef(readNameRef())

// ------ Reading types -----------------------------------------------------

    /** Read names in an interleaved sequence of types/bounds and (parameter) names,
     *  possibly followed by a sequence of modifiers.
     */
    def readParamNamesAndMods(end: Addr): (List[Name], FlagSet) =
      val names =
        collectWhile(currentAddr != end && !isModifierTag(nextByte)) {
          skipTree()
          readName()
        }
      var mods = EmptyFlags
      while currentAddr != end do // avoid boxing the mods
        readByte() match
          case IMPLICIT => mods |= Implicit
          case GIVEN    => mods |= Given
      (names, mods)

    /** Read `n` parameter types or bounds which are interleaved with names */
    def readParamTypes[T <: Type](n: Int)(using Context): List[T] =
      if n == 0 then Nil
      else
        val t = readType().asInstanceOf[T]
        readNat() // skip name
        t :: readParamTypes(n - 1)

    /** Read reference to definition and return symbol created at that definition */
    def readSymRef()(using Context): Symbol = symbolAt(readAddr())

    /** The symbol at given address; create a new one if none exists yet */
    def symbolAt(addr: Addr)(using Context): Symbol = symAtAddr.get(addr) match {
      case Some(sym) =>
        sym
      case None =>
        val sym = forkAt(addr).createSymbol()(using ctx.withOwner(ownerTree.findOwner(addr)))
        report.log(i"forward reference to $sym")
        sym
    }

    /** The symbol defined by current definition */
    def symbolAtCurrent()(using Context): Symbol = symAtAddr.get(currentAddr) match {
      case Some(sym) =>
        assert(ctx.owner == sym.owner, i"owner discrepancy for ${locatedSymbol(sym)}, expected: ${locatedSymbol(ctx.owner)}, found: ${locatedSymbol(sym.owner)}")
        sym
      case None =>
        createSymbol()
    }

    def readConstant(tag: Int)(using Context): Constant = (tag: @switch) match {
      case UNITconst =>
        Constant(())
      case TRUEconst =>
        Constant(true)
      case FALSEconst =>
        Constant(false)
      case BYTEconst =>
        Constant(readInt().toByte)
      case SHORTconst =>
        Constant(readInt().toShort)
      case CHARconst =>
        Constant(readNat().toChar)
      case INTconst =>
        Constant(readInt())
      case LONGconst =>
        Constant(readLongInt())
      case FLOATconst =>
        Constant(java.lang.Float.intBitsToFloat(readInt()))
      case DOUBLEconst =>
        Constant(java.lang.Double.longBitsToDouble(readLongInt()))
      case STRINGconst =>
        Constant(readName().toString)
      case NULLconst =>
        Constant(null)
      case CLASSconst =>
        Constant(readType())
    }

    /** Read a type */
    def readType()(using Context): Type = {
      val start = currentAddr
      val tag = readByte()
      pickling.println(s"reading type ${astTagToString(tag)} at $start, ${ctx.source}")

      def registeringType[T](tp: Type, op: => T): T = {
        typeAtAddr(start) = tp
        op
      }

      def readLengthType(): Type = {
        val end = readEnd()

        def readMethodic[N <: Name, PInfo <: Type, LT <: LambdaType]
            (companionOp: FlagSet => LambdaTypeCompanion[N, PInfo, LT], nameMap: Name => N): LT = {
          val result = typeAtAddr.getOrElse(start, {
              val nameReader = fork
              nameReader.skipTree() // skip result
              val paramReader = nameReader.fork
              val (paramNames, mods) = nameReader.readParamNamesAndMods(end)
              companionOp(mods)(paramNames.map(nameMap))(
                pt => registeringType(pt, paramReader.readParamTypes[PInfo](paramNames.length)),
                pt => readType())
            })
          goto(end)
          result.asInstanceOf[LT]
        }

        def readVariances(tp: Type): Type = tp match
          case tp: HKTypeLambda if currentAddr != end =>
            val vs = until(end) {
              readByte() match
                case STABLE => Invariant
                case COVARIANT => Covariant
                case CONTRAVARIANT => Contravariant
            }
            tp.withVariances(vs)
          case _ => tp

        val result =
          (tag: @switch) match {
            case TERMREFin =>
              var sname = readName()
              val prefix = readType()
              val owner = readType()
              sname match {
                case SignedName(name, sig, target) =>
                  TermRef(prefix, name, owner.decl(name).atSignature(sig, target).asSeenFrom(prefix))
                case name =>
                  TermRef(prefix, name, owner.decl(name).asSeenFrom(prefix))
              }
            case TYPEREFin =>
              val name = readName().toTypeName
              val prefix = readType()
              val space = readType()
              space.decl(name) match {
                case symd: SymDenotation if prefix.isArgPrefixOf(symd.symbol) => TypeRef(prefix, symd.symbol)
                case _ => TypeRef(prefix, name, space.decl(name).asSeenFrom(prefix))
              }
            case REFINEDtype =>
              var name: Name = readName()
              val parent = readType()
              if nextUnsharedTag == TYPEBOUNDS then name = name.toTypeName
              RefinedType(parent, name, readType())
                // Note that the lambda "rt => ..." is not equivalent to a wildcard closure!
                // Eta expansion of the latter puts readType() out of the expression.
            case APPLIEDtype =>
              readType().appliedTo(until(end)(readType()))
            case TYPEBOUNDS =>
              val lo = readType()
              if nothingButMods(end) then AliasingBounds(readVariances(lo))
              else
                val hi = readVariances(readType())
                createNullableTypeBounds(lo, hi)
            case ANNOTATEDtype =>
              AnnotatedType(readType(), Annotation(readTree()))
            case ANDtype =>
              AndType(readType(), readType())
            case ORtype =>
              OrType(readType(), readType(), soft = false)
            case SUPERtype =>
              SuperType(readType(), readType())
            case MATCHtype =>
              MatchType(readType(), readType(), until(end)(readType()))
            case MATCHCASEtype =>
              defn.MatchCaseClass.typeRef.appliedTo(readType(), readType())
            case POLYtype =>
              readMethodic(_ => PolyType, _.toTypeName)
            case METHODtype =>
              def methodTypeCompanion(mods: FlagSet): MethodTypeCompanion =
                if mods.is(Implicit) then ImplicitMethodType
                else if mods.is(Given) then ContextualMethodType
                else MethodType
              readMethodic(methodTypeCompanion, _.toTermName)
            case TYPELAMBDAtype =>
              readMethodic(_ => HKTypeLambda, _.toTypeName)
            case PARAMtype =>
              readTypeRef() match {
                case binder: LambdaType => binder.paramRefs(readNat())
              }
            case FLEXIBLEtype =>
              FlexibleType(readType())
            case _ if isBestEffortTasty =>
              goto(end)
              new PreviousErrorType
          }
        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      def readSimpleType(): Type = (tag: @switch) match {
        case TYPEREFdirect | TERMREFdirect =>
          NamedType(NoPrefix, readSymRef())
        case TYPEREFsymbol | TERMREFsymbol =>
          readSymNameRef()
        case TYPEREFpkg =>
          readPackageRef().moduleClass.typeRef
        case TERMREFpkg =>
          readPackageRef().termRef
        case TYPEREF =>
          val name = readName().toTypeName
          val pre = readType()
          if unpicklingJava && name == tpnme.Object && (pre.termSymbol eq defn.JavaLangPackageVal) then
            defn.FromJavaObjectType
          else
            TypeRef(pre, name)
        case TERMREF =>
          val sname = readName()
          val prefix = readType()
          sname match {
            case SignedName(name, sig, target) =>
              TermRef(prefix, name, prefix.member(name).atSignature(sig, target))
            case name =>
              TermRef(prefix, name)
          }
        case THIS =>
          ThisType.raw(readType().asInstanceOf[TypeRef])
        case RECtype =>
          typeAtAddr.get(start) match {
            case Some(tp) =>
              skipTree(tag)
              tp
            case None =>
              RecType(rt => registeringType(rt, readType()))
          }
        case RECthis =>
          readTypeRef().asInstanceOf[RecType].recThis
        case SHAREDtype =>
          val ref = readAddr()
          typeAtAddr.getOrElseUpdate(ref, forkAt(ref).readType())
        case BYNAMEtype =>
          ExprType(readType())
        case ERRORtype =>
          if isBestEffortTasty then new PreviousErrorType
          else throw new Error(s"Illegal ERRORtype in non Best Effort TASTy file")
        case _ =>
          ConstantType(readConstant(tag))
      }

      if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
    }

    private def readSymNameRef()(using Context): Type = {
      val sym = readSymRef()
      val prefix = readType()
      val res = NamedType(prefix, sym)
      prefix match {
        case prefix: ThisType if (prefix.cls eq sym.owner) && !sym.is(Opaque) =>
          res.withDenot(sym.denot)
          // without this precaution we get an infinite cycle when unpickling pos/extmethods.scala
          // the problem arises when a self type of a trait is a type parameter of the same trait.
        case _ => res
      }
    }

    private def readPackageRef()(using Context): TermSymbol = {
      val name = readName()
      if (name == nme.ROOT || name == nme.ROOTPKG) defn.RootPackage
      else if (name == nme.EMPTY_PACKAGE) defn.EmptyPackageVal
      else requiredPackage(name)
    }

    def readTypeRef(): Type =
      typeAtAddr(readAddr())

    def readTreeRef()(using Context): TermRef =
      readType().asInstanceOf[TermRef]

// ------ Reading definitions -----------------------------------------------------

    private def nothingButMods(end: Addr): Boolean =
      currentAddr == end || isModifierTag(nextByte)

    private def localContext(owner: Symbol)(using Context) =
      ctx.fresh.setOwner(owner)

    private def normalizeFlags(tag: Int, givenFlags: FlagSet, name: Name, isAbsType: Boolean, rhsIsEmpty: Boolean)(using Context): FlagSet = {
      val lacksDefinition =
        rhsIsEmpty &&
          name.isTermName && !name.isConstructorName && !givenFlags.isOneOf(TermParamOrAccessor) ||
        isAbsType
      var flags = givenFlags
      if (lacksDefinition && tag != PARAM) flags |= Deferred
      if (tag == DEFDEF) flags |= Method
      if (givenFlags.is(Module))
        flags |= (if (tag == VALDEF) ModuleValCreationFlags else ModuleClassCreationFlags)
      if flags.is(Enum, butNot = Method) && name.isTermName then
        flags |= StableRealizable
      if name.isTypeName && withCaptureChecks then
        flags |= CaptureChecked
      if (ctx.owner.isClass) {
        if (tag == TYPEPARAM) flags |= Param
        else if (tag == PARAM) {
          flags |= ParamAccessor
          if (!rhsIsEmpty) // param alias
            flags |= Method
        }
      }
      else if (isParamTag(tag)) flags |= Param
      flags
    }

    def isAbstractType(name: Name)(using Context): Boolean = nextByte match
      case SHAREDtype =>
        val lookAhead = fork
        lookAhead.reader.readByte()
        val sharedReader = forkAt(lookAhead.reader.readAddr())
        sharedReader.isAbstractType(name)
      case LAMBDAtpt =>
        val rdr = fork
        rdr.reader.readByte()  // tag
        rdr.reader.readNat()   // length
        rdr.skipParams()       // tparams
        rdr.isAbstractType(name)
      case TYPEBOUNDS =>
        val rdr = fork
        rdr.reader.readByte()  // tag
        val end = rdr.reader.readEnd()
        rdr.skipTree()         // alias, or lower bound
        val res = !rdr.nothingButMods(end)
        //if !res then println(i"NOT ABSTRACT $name, ${rdr.reader.nextByte}")
        res
      case TYPEBOUNDStpt => true
      case _ => false

    /** Create symbol of definition node and enter in symAtAddr map
     *  @return  the created symbol
     */
    def createSymbol()(using Context): Symbol = nextByte match {
      case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
        createMemberSymbol()
      case BIND =>
        createBindSymbol()
      case TEMPLATE =>
        val localDummy = newLocalDummy(ctx.owner)
        registerSym(currentAddr, localDummy)
        localDummy
      case tag =>
        throw new Error(s"illegal createSymbol at $currentAddr, tag = $tag")
    }

    private def createBindSymbol()(using Context): Symbol = {
      val start = currentAddr
      val tag = readByte()
      val end = readEnd()
      var name: Name = readName()
      if nextUnsharedTag == TYPEBOUNDS then name = name.toTypeName
      val typeReader = fork
      val completer = new LazyType {
        def complete(denot: SymDenotation)(using Context) =
          denot.info = typeReader.readType()
      }
      val sym = newSymbol(ctx.owner, name, Flags.Case, completer, coord = coordAt(start))
      registerSym(start, sym)
      sym
    }

    /** Create symbol of member definition or parameter node and enter in symAtAddr map
     *  @return  the created symbol
     */
    def createMemberSymbol()(using Context): Symbol = {
      val start = currentAddr
      val tag = readByte()
      val end = readEnd()
      var name: Name = readName()
      if (tag == TYPEDEF || tag == TYPEPARAM) name = name.toTypeName
      skipParams()
      val ttag = nextUnsharedTag
      val isAbsType = isAbstractType(name)
      val isClass = ttag == TEMPLATE
      val templateStart = currentAddr
      skipTree() // tpt
      val rhsStart = currentAddr
      val rhsIsEmpty = nothingButMods(end)
      if (!rhsIsEmpty) skipTree()
      val (givenFlags0, annotFns, privateWithin) = readModifiers(end)
      val givenFlags =
        if isClass && unpicklingScala2Library then givenFlags0 | Scala2x | Scala2Tasty
        else if unpicklingJava then givenFlags0 | JavaDefined
        else givenFlags0
      pickling.println(i"creating symbol $name at $start with flags ${givenFlags.flagsString}, isAbsType = $isAbsType, $ttag")
      val flags = normalizeFlags(tag, givenFlags, name, isAbsType, rhsIsEmpty)
      def adjustIfModule(completer: LazyType) =
        if (flags.is(Module)) adjustModuleCompleter(completer, name) else completer
      val coord = coordAt(start)
      val sym =
        roots.find(root => (root.owner eq ctx.owner) && root.name == name) match {
          case Some(rootd) =>
            pickling.println(i"overwriting ${rootd.symbol} # ${rootd.hashCode}")
            rootd.symbol.coord = coord
            rootd.info = adjustIfModule(
                new Completer(subReader(start, end)) with SymbolLoaders.SecondCompleter)
            rootd.flags = flags &~ Touched // allow one more completion
            rootd.setPrivateWithin(privateWithin)
            seenRoots += rootd.symbol
            rootd.symbol
          case _ =>
            val completer = adjustIfModule(new Completer(subReader(start, end)))
            if isClass then
              newClassSymbol(ctx.owner, name.asTypeName, flags, completer, privateWithin, coord, compilationUnitInfo)
            else
              newSymbol(ctx.owner, name, flags, completer, privateWithin, coord)
        }
      val annotOwner =
        if sym.owner.isClass then newLocalDummy(sym.owner) else sym.owner
      val annots = annotFns.map(_(annotOwner))
      sym.annotations = annots
      if sym.isOpaqueAlias then sym.setFlag(Deferred)
      val isScala2MacroDefinedInScala3 = flags.is(Macro, butNot = Inline) && flags.is(Erased)
      ctx.owner match {
        case cls: ClassSymbol if !isScala2MacroDefinedInScala3 || cls == defn.StringContextClass =>
          // Enter all members of classes that are not Scala 2 macros.
          //
          // For `StringContext`, enter `s`, `f` and `raw`
          // These definitions will be entered when defined in Scala 2. It is fine to enter them
          // as they are intrinsic macros and are specially handled by the compiler.
          // Dual macro definitions will not work on `StringContext` as we would enter the symbol twice.
          // But dual macros will never be needed for those definitions due to their intinsic nature.
          cls.enter(sym)
        case _ =>
      }
      registerSym(start, sym)
      if (isClass) {
        if sym.owner.is(Package) && withCaptureChecks then
          sym.setFlag(CaptureChecked)
        sym.completer.withDecls(newScope)
        forkAt(templateStart).indexTemplateParams()(using localContext(sym))
      }
      else if (sym.isInlineMethod && !sym.is(Deferred))
        sym.addAnnotation(LazyBodyAnnotation { (ctx0: Context) ?=>
          val ctx1 = localContext(sym)(using ctx0).addMode(Mode.ReadPositions)
          inContext(sourceChangeContext(Addr(0))(using ctx1)) {
            // avoids space leaks by not capturing the current context
            forkAt(rhsStart).readTree()
          }
        })
      goto(start)
      sym
    }

    /** Read modifier list into triplet of flags, annotations and a privateWithin
     *  boundary symbol.
     */
    def readModifiers(end: Addr)(using Context): (FlagSet, List[Symbol => Annotation], Symbol) = {
      var flags: FlagSet = EmptyFlags
      var annotFns: List[Symbol => Annotation] = Nil
      var privateWithin: Symbol = NoSymbol
      while (currentAddr.index != end.index) {
        def addFlag(flag: FlagSet) = {
          flags |= flag
          readByte()
        }
        nextByte match {
          case PRIVATE => addFlag(Private)
          case PROTECTED => addFlag(Protected)
          case ABSTRACT =>
            readByte()
            nextByte match {
              case OVERRIDE => addFlag(AbsOverride)
              case _ => flags |= Abstract
            }
          case FINAL => addFlag(Final)
          case SEALED => addFlag(Sealed)
          case CASE => addFlag(Case)
          case IMPLICIT => addFlag(Implicit)
          case ERASED => addFlag(Erased)
          case LAZY => addFlag(Lazy)
          case OVERRIDE => addFlag(Override)
          case INLINE => addFlag(Inline)
          case INLINEPROXY => addFlag(InlineProxy)
          case MACRO => addFlag(Macro)
          case OPAQUE => addFlag(Opaque)
          case STATIC => addFlag(JavaStatic)
          case OBJECT => addFlag(Module)
          case TRAIT => addFlag(Trait)
          case ENUM => addFlag(Enum)
          case LOCAL => addFlag(Local)
          case SYNTHETIC => addFlag(Synthetic)
          case ARTIFACT => addFlag(Artifact)
          case MUTABLE => addFlag(Mutable)
          case FIELDaccessor => addFlag(Accessor)
          case CASEaccessor => addFlag(CaseAccessor)
          case COVARIANT => addFlag(Covariant)
          case CONTRAVARIANT => addFlag(Contravariant)
          case HASDEFAULT => addFlag(HasDefault)
          case STABLE => addFlag(StableRealizable)
          case EXTENSION => addFlag(Extension)
          case GIVEN => addFlag(Given)
          case PARAMsetter => addFlag(ParamAccessor)
          case PARAMalias => addFlag(SuperParamAlias)
          case EXPORTED => addFlag(Exported)
          case OPEN => addFlag(Open)
          case INVISIBLE => addFlag(Invisible)
          case TRANSPARENT => addFlag(Transparent)
          case INFIX => addFlag(Infix)
          case PRIVATEqualified =>
            readByte()
            privateWithin = readWithin
          case PROTECTEDqualified =>
            addFlag(Protected)
            privateWithin = readWithin
          case ANNOTATION =>
            annotFns = readAnnot :: annotFns
          case tag =>
            assert(false, s"illegal modifier tag $tag at $currentAddr, end = $end")
        }
      }
      (flags, annotFns.reverse, privateWithin)
    }

    private def readWithin(using Context): Symbol = readType().typeSymbol

    private def readAnnot(using Context): Symbol => Annotation =
      readByte()
      val end = readEnd()
      val tp = readType()
      val lazyAnnotTree = readLaterWithOwner(end, _.readTree())
      owner =>
        new DeferredSymAndTree(tp.typeSymbol, lazyAnnotTree(owner).complete):
          // Only force computation of symbol if it has the right name. This added
          // amount of laziness is sometimes necessary to avid cycles. Test case pos/i15980.
          override def hasSymbol(sym: Symbol)(using Context) = tp match
            case tp: TypeRef =>
              tp.designator match
                case name: Name => name == sym.name && tp.symbol == sym
                case _ => tp.symbol == sym
            case _ => this.symbol == sym

    /** Create symbols for the definitions in the statement sequence between
     *  current address and `end`.
     *  @return  the largest subset of {NoInits, PureInterface} that a
     *           trait owning the indexed statements can have as flags.
     */
    def indexStats(end: Addr)(using Context): FlagSet = {
      var initsFlags = NoInitsInterface
      while (currentAddr.index < end.index)
        nextByte match {
          case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
            val sym = symbolAtCurrent()
            skipTree()
            if (sym.isTerm && !sym.isOneOf(DeferredOrLazyOrMethod))
              initsFlags = EmptyFlags
            else if (sym.isClass ||
              sym.is(Method, butNot = Deferred) && !sym.isConstructor)
              initsFlags &= NoInits
          case IMPORT | EXPORT =>
            skipTree()
          case PACKAGE =>
            processPackage { (pid, end) => indexStats(end) }
          case _ =>
            skipTree()
            initsFlags = EmptyFlags
        }
      assert(currentAddr.index == end.index)
      initsFlags
    }

    /** Process package with given operation `op`. The operation takes as arguments
     *   - a `RefTree` representing the `pid` of the package,
     *   - an end address,
     *   - a context which has the processed package as owner
     */
    def processPackage[T](op: (RefTree, Addr) => Context ?=> T)(using Context): T = {
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return processPackage(op)(using sctx)
      readByte()
      val end = readEnd()
      val pid = ref(readTreeRef()).asInstanceOf[RefTree]
      op(pid, end)(using localContext(pid.symbol.moduleClass))
    }

    /** Create symbols the longest consecutive sequence of parameters with given
     *  `tag` starting at current address.
     */
    def indexParams(tag: Int)(using Context): Unit =
      while (nextByte == tag) {
        symbolAtCurrent()
        skipTree()
      }

    /** Create symbols for all type and value parameters of template starting
     *  at current address.
     */
    def indexTemplateParams()(using Context): Unit = {
      assert(readByte() == TEMPLATE)
      readEnd()
      indexParams(TYPEPARAM)
      indexParams(PARAM)
    }

    /** If definition was already read by a completer, return the previously read tree
     *  or else read definition.
     */
    def readIndexedDef()(using Context): Tree = treeAtAddr.remove(currentAddr) match {
      case Some(tree) =>
        assert(tree != PoisonTree, s"Cyclic reference while unpickling definition at address ${currentAddr.index} in unit ${ctx.compilationUnit}")
        skipTree()
        tree
      case none =>
        val start = currentAddr
        treeAtAddr(start) = PoisonTree
        val tree = readNewDef()
        treeAtAddr.remove(start)
        tree
    }

    private def readNewDef()(using Context): Tree = {
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return readNewDef()(using sctx)
      val start = currentAddr
      val sym = symAtAddr(start)
      val tag = readByte()
      val end = readEnd()

      def readParamss()(using Context): List[ParamClause] =
        def readRest() =
          if nextByte == SPLITCLAUSE then readByte()
          readParamss()
        nextByte match
          case PARAM => readParams[ValDef](PARAM) :: readRest()
          case TYPEPARAM => readParams[TypeDef](TYPEPARAM) :: readRest()
          case EMPTYCLAUSE => readByte(); Nil :: readRest()
          case _ => Nil

      val localCtx = localContext(sym)

      def readRhs(using Context): LazyTree =
        if (nothingButMods(end))
          EmptyTree
        else if sym.isInlineMethod && !sym.is(Deferred) then
          // The body of an inline method is stored in an annotation, so no need to unpickle it again
          new Trees.Lazy[Tree] {
            def complete(using Context) = inlines.Inlines.bodyToInline(sym)
          }
        else
          readLater(end, _.readTree())

      def ValDef(tpt: Tree) =
        ta.assignType(untpd.ValDef(sym.name.asTermName, tpt, readRhs(using localCtx)), sym)

      def DefDef(paramss: List[ParamClause], tpt: Tree) =
        sym.setParamssFromDefs(paramss)
        ta.assignType(
          untpd.DefDef(sym.name.asTermName, paramss, tpt, readRhs(using localCtx)),
          sym)

      def TypeDef(rhs: Tree) =
        ta.assignType(untpd.TypeDef(sym.name.asTypeName, rhs), sym)

      def ta = ctx.typeAssigner

      val name = readName()
      pickling.println(s"reading def of $name at $start")
      val tree: MemberDef = tag match {
        case DEFDEF =>
          val paramDefss = readParamss()(using localCtx)
          val tpt = readTpt()(using localCtx)
          val paramss = normalizeIfConstructor(
              paramDefss.nestedMap(_.symbol), name == nme.CONSTRUCTOR)
          val resType =
            if name == nme.CONSTRUCTOR then
              effectiveResultType(sym, paramss)
            else
              tpt.tpe
          sym.info = methodType(paramss, resType)
          DefDef(paramDefss, tpt)
        case VALDEF =>
          val tpt = readTpt()(using localCtx)
          sym.info = tpt.tpe
          ValDef(tpt)
        case TYPEDEF | TYPEPARAM =>
          if (sym.isClass) {
            sym.owner.ensureCompleted() // scalacLinkedClass uses unforcedDecls. Make sure it does not miss anything.
            val companion = sym.scalacLinkedClass

            // Is the companion defined in the same Tasty file as `sym`?
            // The only case to check here is if `sym` is a root. In this case
            // `companion` might have been entered by the environment but it might
            // be missing from the Tasty file. So we check explicitly for that.
            def isCodefined = roots.contains(companion.denot) == seenRoots.contains(companion)

            if (companion.exists && isCodefined) sym.registerCompanion(companion)
            TypeDef(readTemplate(using localCtx))
          }
          else {
            sym.info = TypeBounds.empty // needed to avoid cyclic references when unpickling rhs, see i3816.scala
            sym.setFlag(Provisional)
            val rhs = readTpt()(using localCtx)

            sym.info = new NoCompleter:
              override def complete(denot: SymDenotation)(using Context): Unit = if !isBestEffortTasty then unsupported("complete")
              override def completerTypeParams(sym: Symbol)(using Context) =
                rhs.tpe.typeParams

            def opaqueToBounds(info: Type): Type =
              val tparamSyms = rhs match
                case LambdaTypeTree(tparams, body) => tparams.map(_.symbol.asType)
                case _ => Nil
              sym.opaqueToBounds(info, rhs, tparamSyms)

            val info = checkNonCyclic(sym, rhs.tpe.toBounds, reportErrors = false)
            if sym.isOpaqueAlias then
              sym.info = opaqueToBounds(info)
              sym.typeRef.recomputeDenot() // make sure we see the new bounds from now on
            else
              sym.info = info

            sym.resetFlag(Provisional)
            TypeDef(rhs)
          }
        case PARAM =>
          val tpt = readTpt()(using localCtx)
          assert(nothingButMods(end))
          sym.info = tpt.tpe
          ValDef(tpt)
      }
      goto(end)
      setSpan(start, tree)

      // Dealias any non-accessible type alias in the type of `sym`. This can be
      // skipped for types (see `checkNoPrivateLeaks` for why) as well as for
      // param accessors since they can't refer to an inaccesible type member of
      // the class.
      if !sym.isType && !sym.is(ParamAccessor) then
        sym.info = ta.avoidPrivateLeaks(sym)

      if (ctx.settings.YreadComments.value) {
        assert(ctx.docCtx.isDefined, "`-Yread-docs` enabled, but no `docCtx` is set.")
        commentUnpicklerOpt.foreach { commentUnpickler =>
          val comment = commentUnpickler.commentAt(start)
          ctx.docCtx.get.addDocstring(tree.symbol, comment)
          tree.setComment(comment)
        }
      }

      tree.setDefTree
    }

    /** Read enough of parent to determine its type, without reading arguments
     *  of applications. This is necessary to make TreeUnpickler as lazy as Namer
     *  in this regard. See i16673 for a test case.
     */
    private def readParentType()(using Context): Type =
      readByte() match
        case TYPEAPPLY =>
          val end = readEnd()
          val tycon = readParentType()
          if tycon.typeParams.isEmpty then
            goto(end)
            tycon
          else
            val args = until(end)(readTpt())
            val cls = tycon.classSymbol
            assert(cls.typeParams.hasSameLengthAs(args))
            cls.typeRef.appliedTo(args.tpes)
        case APPLY | BLOCK =>
          val end = readEnd()
          try readParentType()
          finally goto(end)
        case SELECTin =>
          val end = readEnd()
          readName()
          readTree() match
            case nu: New =>
              try nu.tpe
              finally goto(end)
            case other if isBestEffortTasty =>
              try other.tpe
              finally goto(end)
        case SHAREDterm =>
          forkAt(readAddr()).readParentType()
        case SELECT if isBestEffortTasty =>
          goto(readEnd())
          new PreviousErrorType

    /** Read template parents
     *  @param  withArgs if false, only read enough of parent trees to determine their type
     *                   but skip constructor arguments. Return any trees that were partially
     *                   parsed in this way as InferredTypeTrees.
     */
    def readParents(withArgs: Boolean)(using Context): List[Tree] =
      collectWhile({val tag = nextByte; tag != SELFDEF && tag != DEFDEF && tag != SPLITCLAUSE}) {
        nextUnsharedTag match
          case APPLY | TYPEAPPLY | BLOCK =>
            if withArgs then readTree()
            else InferredTypeTree().withType(readParentType())
          case _ => readTpt()
      }

    private def readTemplate(using Context): Template = {
      val start = currentAddr
      assert(sourcePathAt(start).isEmpty)
      val cls = ctx.owner.asClass
      val assumedSelfType =
        if (cls.is(Module) && cls.owner.isClass) TermRef(cls.owner.thisType, cls.name.sourceModuleName)
        else NoType
      cls.info = new TempClassInfo(cls.owner.thisType, cls, cls.unforcedDecls, assumedSelfType)
      val localDummy = symbolAtCurrent()
      val parentCtx = ctx.withOwner(localDummy)
      assert(readByte() == TEMPLATE)
      val end = readEnd()
      val tparams = readIndexedParams[TypeDef](TYPEPARAM)
      val vparams = readIndexedParams[ValDef](PARAM)
      // It's important to index the class definitions before unpickling the parents
      // (see the parents-cycle test for examples where this matter)
      val bodyFlags = {
        val bodyIndexer = fork
        // The first DEFDEF corresponds to the primary constructor
        while ({val tag = bodyIndexer.reader.nextByte; tag != DEFDEF && tag != SPLITCLAUSE}) do
          bodyIndexer.skipParentTree()
        bodyIndexer.indexStats(end)
      }
      val parentReader = fork
      val parents = readParents(withArgs = false)(using parentCtx)
      val parentTypes = parents.map(_.tpe.dealias)
      if cls.is(JavaDefined) && parentTypes.exists(_.derivesFrom(defn.JavaAnnotationClass)) then
        cls.setFlag(JavaAnnotation)
      val self =
        if (nextByte == SELFDEF) {
          readByte()
          untpd.ValDef(readName(), readTpt(), EmptyTree).withType(NoType)
        }
        else EmptyValDef
      cls.setNoInitsFlags(parentsKind(parents), bodyFlags)
      cls.info = ClassInfo(
          cls.owner.thisType, cls, parentTypes, cls.unforcedDecls,
          selfInfo = if (self.isEmpty) NoType else self.tpt.tpe
        ).integrateOpaqueMembers

      val constr =
        if nextByte == SPLITCLAUSE then
          assert(unpicklingJava, s"unexpected SPLITCLAUSE at $start")
          val tag = readByte()
          def ta = ctx.typeAssigner
          val flags = Flags.JavaDefined | Flags.PrivateLocal | Flags.Invisible
          val ctorCompleter = new LazyType {
            def complete(denot: SymDenotation)(using Context) =
              val sym = denot.symbol
              val pflags = flags | Flags.Param
              val tparamRefs = tparams.map(_.symbol.asType)
              lazy val derivedTparamSyms: List[TypeSymbol] = tparams.map: tdef =>
                val completer = new LazyType {
                  def complete(denot: SymDenotation)(using Context) =
                    denot.info = tdef.symbol.asType.info.subst(tparamRefs, derivedTparamRefs)
                }
                newSymbol(sym, tdef.name, Flags.JavaDefined | Flags.Param, completer, coord = cls.coord)
              lazy val derivedTparamRefs: List[Type] = derivedTparamSyms.map(_.typeRef)
              val vparamSym =
                newSymbol(sym, nme.syntheticParamName(1), pflags, defn.UnitType, coord = cls.coord)
              val vparamSymss: List[List[Symbol]] = List(vparamSym) :: Nil
              val paramSymss =
                if derivedTparamSyms.nonEmpty then derivedTparamSyms :: vparamSymss else vparamSymss
              val res = effectiveResultType(sym, paramSymss)
              denot.info = methodType(paramSymss, res)
              denot.setParamss(paramSymss)
          }
          val ctorSym = newSymbol(ctx.owner, nme.CONSTRUCTOR, flags, ctorCompleter, coord = coordAt(start))
          tpd.DefDef(ctorSym, EmptyTree).setDefTree // fake primary constructor
        else
          readIndexedDef().asInstanceOf[DefDef]
      val mappedParents: LazyTreeList =
        if parents.exists(_.isInstanceOf[InferredTypeTree]) then
          // parents were not read fully, will need to be read again later on demand
          new LazyReader(parentReader, localDummy, ctx.mode, ctx.source,
            _.readParents(withArgs = true)
             .map(_.changeOwner(localDummy, constr.symbol)))
        else parents

      val lazyStats = readLater(end, rdr => {
        val stats = rdr.readIndexedStats(localDummy, end)
        tparams ++ vparams ++ stats
      })
      defn.patchStdLibClass(cls)
      NamerOps.addConstructorProxies(cls)
      setSpan(start,
        untpd.Template(constr, mappedParents, self, lazyStats)
          .withType(localDummy.termRef))
    }

    def skipToplevel()(using Context): Unit= {
      if (!isAtEnd && isTopLevel) {
        skipTree()
        skipToplevel()
      }
    }

    def isTopLevel(using Context): Boolean =
      nextByte == IMPORT || nextByte == PACKAGE

    def readTopLevel()(using Context): List[Tree] = {
      @tailrec def read(acc: ListBuffer[Tree]): List[Tree] =
        if (isTopLevel) {
          acc += readIndexedStat(NoSymbol)
          if (!isAtEnd) read(acc) else acc.toList
        }
        else // top-level trees which are not imports or packages are not part of tree
          acc.toList
      read(new ListBuffer[tpd.Tree])
    }

    def readIndexedStat(exprOwner: Symbol)(using Context): Tree = nextByte match {
      case TYPEDEF | VALDEF | DEFDEF =>
        readIndexedDef()
      case IMPORT =>
        readImportOrExport(Import(_, _))()
      case EXPORT =>
        readImportOrExport(Export(_, _))()
      case PACKAGE =>
        val start = currentAddr
        processPackage { (pid, end) =>
          setSpan(start, PackageDef(pid, readIndexedStats(exprOwner, end)))
        }
      case _ =>
        readTree()(using ctx.withOwner(exprOwner))
    }

    inline def readImportOrExport(inline mkTree:
        (Tree, List[untpd.ImportSelector]) => Tree)()(using Context): Tree = {
      val start = currentAddr
      assert(sourcePathAt(start).isEmpty)
      readByte()
      readEnd()
      val expr = readTree()
      setSpan(start, mkTree(expr, readSelectors()))
    }

    def readSelectors()(using Context): List[untpd.ImportSelector] =
      if nextByte == IMPORTED then
        val start = currentAddr
        assert(sourcePathAt(start).isEmpty)
        readByte()
        val name = readName()
        val from = setSpan(start, untpd.Ident(name))
        val selector = nextByte match
          case RENAMED =>
            val start2 = currentAddr
            readByte()
            val to = setSpan(start2, untpd.Ident(readName()))
            untpd.ImportSelector(from, to, EmptyTree)
          case BOUNDED =>
            val start2 = currentAddr
            readByte()
            val bound = setSpan(start2, untpd.TypedSplice(readTpt()))
            untpd.ImportSelector(from, EmptyTree, bound)
          case _ =>
            untpd.ImportSelector(from)
        selector :: readSelectors()
      else
        Nil

    def readIndexedStats[T](exprOwner: Symbol, end: Addr, k: (List[Tree], Context) => T = sameTrees)(using Context): T =
      val buf = new mutable.ListBuffer[Tree]
      var curCtx = ctx
      while currentAddr.index < end.index do
        val stat = readIndexedStat(exprOwner)(using curCtx)
        buf += stat
        stat match
          case stat: Import => curCtx = curCtx.importContext(stat, stat.symbol)
          case _ =>
      assert(currentAddr.index == end.index)
      k(buf.toList, curCtx)

    def readStats[T](exprOwner: Symbol, end: Addr, k: (List[Tree], Context) => T = sameTrees)(using Context): T = {
      fork.indexStats(end)
      readIndexedStats(exprOwner, end, k)
    }

    private def sameTrees(xs: List[Tree], ctx: Context) = xs

    def readIndexedParams[T <: MemberDef](tag: Int)(using Context): List[T] =
      collectWhile(nextByte == tag) { readIndexedDef().asInstanceOf[T] }

    def readParams[T <: MemberDef](tag: Int)(using Context): List[T] =
      if nextByte == tag then
        fork.indexParams(tag)
        readIndexedParams(tag)
      else Nil

// ------ Reading trees -----------------------------------------------------

    private def ElidedTree(tpe: Type)(using Context): Tree =
      untpd.Ident(nme.WILDCARD).withType(tpe)

    def readTree()(using Context): Tree = {
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return readTree()(using sctx)
      val start = currentAddr
      val tag = readByte()
      pickling.println(s"reading term ${astTagToString(tag)} at $start, ${ctx.source}")

      def readPathTree(): Tree = {
        goto(start)
        readType() match {
          case path: TypeRef => TypeTree(path)
          case path: TermRef => ref(path)
          case path: ThisType => untpd.This(untpd.EmptyTypeIdent).withType(path)
          case path: ConstantType => Literal(path.value)
          case path: ErrorType if isBestEffortTasty => TypeTree(path)
        }
      }

      def makeSelect(qual: Tree, name: Name, denot: Denotation): Select =
        var qualType = qual.tpe.widenIfUnstable
        val owner = denot.symbol.maybeOwner
        val tpe0 = name match
          case name: TypeName => TypeRef(qualType, name, denot)
          case name: TermName => TermRef(qualType, name, denot)
        val tpe = TypeOps.makePackageObjPrefixExplicit(tpe0)
        ConstFold.Select(untpd.Select(qual, name).withType(tpe))

      def completeSelect(name: Name, sig: Signature, target: Name): Select =
        val qual = readTree()
        val denot =
          if unpicklingJava && name == tpnme.Object && qual.symbol == defn.JavaLangPackageVal then
            defn.FromJavaObjectSymbol.denot
          else
            accessibleDenot(qual.tpe.widenIfUnstable, name, sig, target)
        makeSelect(qual, name, denot)

      def readQualId(): (untpd.Ident, TypeRef) =
        val qual = readTree().asInstanceOf[untpd.Ident]
        (untpd.Ident(qual.name).withSpan(qual.span), qual.tpe.asInstanceOf[TypeRef])

      def accessibleDenot(qualType: Type, name: Name, sig: Signature, target: Name) = {
        val pre = ctx.typeAssigner.maybeSkolemizePrefix(qualType, name)
        val d = qualType.findMember(name, pre).atSignature(sig, target)
        if (!d.symbol.exists || d.symbol.isAccessibleFrom(pre)) d
        else qualType.findMember(name, pre, excluded = Private).atSignature(sig, target)
      }

      def readSimpleTree(): Tree = tag match {
        case SHAREDterm =>
          forkAt(readAddr()).readTree()
        case IDENT =>
          untpd.Ident(readName()).withType(readType())
        case ELIDED =>
          if !isOutline then
            val msg =
              s"Illegal elided tree in unpickler at $start without ${attributeTagToString(OUTLINEattr)}, ${ctx.source}"
            report.error(msg)
          ElidedTree(readType())
        case IDENTtpt =>
          untpd.Ident(readName().toTypeName).withType(readType())
        case SELECT =>
          readName() match {
            case SignedName(name, sig, target) => completeSelect(name, sig, target)
            case name => completeSelect(name, Signature.NotAMethod, EmptyTermName)
          }
        case SELECTtpt =>
          val name = readName().toTypeName
          completeSelect(name, Signature.NotAMethod, EmptyTermName)
        case QUALTHIS =>
          val (qual, tref) = readQualId()
          untpd.This(qual).withType(ThisType.raw(tref))
        case NEW =>
          New(readTpt())
        case THROW =>
          Throw(readTree())
        case SINGLETONtpt =>
          SingletonTypeTree(readTree())
        case BYNAMEtpt =>
          ByNameTypeTree(readTpt())
        case NAMEDARG =>
          NamedArg(readName(), readTree())
        case EXPLICITtpt =>
          readTpt()
        case _ =>
          readPathTree()
      }

      /** Adapt constructor calls where class has only using clauses from old to new scheme.
       *  or class has mixed using clauses and other clauses.
       *  Old: leading (), new: nothing, or trailing () if all clauses are using clauses.
       *  This is neccessary so that we can read pre-3.2 Tasty correctly. There,
       *  constructor calls use the old scheme, but constructor definitions already
       *  use the new scheme, since they are reconstituted with normalizeIfConstructor.
       */
      def constructorApply(fn: Tree, args: List[Tree]): Tree =
        if fn.tpe.widen.isContextualMethod && args.isEmpty then
          fn.withAttachment(SuppressedApplyToNone, ())
        else
          val fn1 = fn match
            case Apply(fn1, Nil) if fn.removeAttachment(InsertedApplyToNone).isDefined =>
              // We thought we inserted a final `()` but hit a user-written `()` instead.
              // Remove the inserted `()`.
              fn1
            case _ =>
              fn
          val res = tpd.Apply(fn1, args)
          if fn.removeAttachment(SuppressedApplyToNone).isEmpty then
            res
          else res.tpe.widen match
            case mt @ MethodType(params) =>
              if params.isEmpty then
                // Assume it's the final synthesized `()` parameter
                res.appliedToNone.withAttachment(InsertedApplyToNone, ())
              else if mt.isContextualMethod then
                res.withAttachment(SuppressedApplyToNone, ())
              else res

      def quotedExpr(fn: Tree, args: List[Tree]): Tree =
        val TypeApply(_, targs) = fn: @unchecked
        untpd.Quote(args.head, Nil).withBodyType(targs.head.tpe)

      def splicedExpr(fn: Tree, args: List[Tree]): Tree =
        val TypeApply(_, targs) = fn: @unchecked
        Splice(args.head, targs.head.tpe)

      def nestedSpliceExpr(fn: Tree, args: List[Tree]): Tree =
        fn match
          case Apply(TypeApply(_, targs), _ :: Nil) => // nestedSplice[T](quotes)(expr)
            Splice(args.head, targs.head.tpe)
          case _ => // nestedSplice[T](quotes)
            tpd.Apply(fn, args)

      def simplifyLub(tree: Tree): Tree =
        tree.overwriteType(tree.tpe.simplified)
        tree

      def readLengthTree(): Tree = {
        val end = readEnd()
        val result =
          (tag: @switch) match {
            case SUPER =>
              val qual = readTree()
              val (mixId, mixTpe) = ifBefore(end)(readQualId(), (untpd.EmptyTypeIdent, NoType))
              tpd.Super(qual, mixId, mixTpe.typeSymbol)
            case APPLY =>
              val fn = readTree()
              val args = until(end)(readTree())
              if fn.symbol.isConstructor then constructorApply(fn, args)
              else if fn.symbol == defn.QuotedRuntime_exprQuote then quotedExpr(fn, args)
              else if fn.symbol == defn.QuotedRuntime_exprSplice then splicedExpr(fn, args)
              else if fn.symbol == defn.QuotedRuntime_exprNestedSplice then nestedSpliceExpr(fn, args)
              else tpd.Apply(fn, args)
            case TYPEAPPLY =>
              tpd.TypeApply(readTree(), until(end)(readTpt()))
            case APPLYsigpoly =>
              val fn = readTree()
              val methType = readType()
              val args = until(end)(readTree())
              val fun2 = typer.Applications.retypeSignaturePolymorphicFn(fn, methType)
              tpd.Apply(fun2, args)
            case TYPED =>
              val expr = readTree()
              val tpt = readTpt()
              Typed(expr, tpt)
            case ASSIGN =>
              Assign(readTree(), readTree())
            case BLOCK =>
              val exprReader = fork
              skipTree()
              readStats(ctx.owner, end,
                (stats, ctx) => Block(stats, exprReader.readTree()(using ctx)))
            case INLINED =>
              val exprReader = fork
              skipTree()
              def maybeCall = nextUnsharedTag match {
                case VALDEF | DEFDEF => EmptyTree
                case _ => readTree()
              }
              val call = ifBefore(end)(maybeCall, EmptyTree)
              val bindings = readStats(ctx.owner, end).asInstanceOf[List[ValOrDefDef]]
              val expansion = exprReader.readTree() // need bindings in scope, so needs to be read before
              Inlined(call, bindings, expansion)
            case IF =>
              if (nextByte == INLINE) {
                readByte()
                InlineIf(readTree(), readTree(), readTree())
              }
              else
                If(readTree(), readTree(), readTree())
            case LAMBDA =>
              val meth = readTree()
              val tpt = ifBefore(end)(readTpt(), EmptyTree)
              Closure(Nil, meth, tpt)
            case MATCH =>
              simplifyLub(
                if (nextByte == IMPLICIT) {
                  readByte()
                  InlineMatch(EmptyTree, readCases(end))
                }
                else if (nextByte == INLINE) {
                  readByte()
                  InlineMatch(readTree(), readCases(end))
                }
                else Match(readTree(), readCases(end)))
            case RETURN =>
              val from = readSymRef()
              val expr = ifBefore(end)(readTree(), EmptyTree)
              Return(expr, Ident(from.termRef))
            case WHILE =>
              WhileDo(readTree(), readTree())
            case TRY =>
              simplifyLub(
                Try(readTree(), readCases(end), ifBefore(end)(readTree(), EmptyTree)))
            case SELECTouter =>
              val levels = readNat()
              readTree().outerSelect(levels, SkolemType(readType()))
            case SELECTin =>
              var sname = readName()
              val qual = readTree()
              val ownerTpe = readType()
              val owner = ownerTpe.typeSymbol
              val SignedName(name, sig, target) = sname: @unchecked // only methods with params use SELECTin
              val qualType = qual.tpe.widenIfUnstable
              val prefix = ctx.typeAssigner.maybeSkolemizePrefix(qualType, name)

              /** Tasty should still be able to resolve a method from another root class,
               *  even if it has been moved to a super type,
               *  or an override has been removed.
               *
               *  This is tested in
               *  - sbt-test/tasty-compat/remove-override
               *  - sbt-test/tasty-compat/move-method
               */
              def lookupInSuper =
                val cls = ownerTpe.classSymbol
                if cls.exists then
                  cls.asClass.classDenot
                    .findMember(name, cls.thisType, EmptyFlags, excluded=Private)
                    .atSignature(sig, target)
                else
                  NoDenotation

              val denot =
                val d = ownerTpe.decl(name).atSignature(sig, target)
                (if !d.exists then lookupInSuper else d).asSeenFrom(prefix)

              makeSelect(qual, name, denot)
            case REPEATED =>
              val elemtpt = readTpt()
              SeqLiteral(until(end)(readTree()), elemtpt)
            case BIND =>
              val sym = symAtAddr.getOrElse(start, forkAt(start).createSymbol())
              readName()
              readType()
              val body = readTree()
              val (givenFlags, _, _) = readModifiers(end)
              sym.setFlag(givenFlags)
              Bind(sym, body)
            case ALTERNATIVE =>
              Alternative(until(end)(readTree()))
            case UNAPPLY =>
              val fn = readTree()
              val implicitArgs =
                collectWhile(nextByte == IMPLICITarg) {
                  readByte()
                  readTree()
                }
              val patType = readType()
              val argPats = until(end)(readTree())
              val unapply = UnApply(fn, implicitArgs, argPats, patType)
              if fn.symbol == defn.QuoteMatching_ExprMatch_unapply
                 || fn.symbol == defn.QuoteMatching_TypeMatch_unapply
              then QuotePatterns.decode(unapply)
              else unapply
            case REFINEDtpt =>
              val refineCls = symAtAddr.getOrElse(start,
                newRefinedClassSymbol(coordAt(start))).asClass
              registerSym(start, refineCls)
              typeAtAddr(start) = refineCls.typeRef
              val parent = readTpt()
              val refinements = readStats(refineCls, end)(using localContext(refineCls))
              RefinedTypeTree(parent, refinements, refineCls)
            case APPLIEDtpt =>
              // If we do directly a tpd.AppliedType tree we might get a
              // wrong number of arguments in some scenarios reading F-bounded
              // types. This came up in #137 of collection strawman.
              val tycon = readTpt()
              val args = until(end)(readTpt())
              val tree = untpd.AppliedTypeTree(tycon, args)
              val ownType = ctx.typeAssigner.processAppliedType(tree, tycon.tpe.safeAppliedTo(args.tpes))
              tree.withType(ownType)
            case ANNOTATEDtpt =>
              Annotated(readTpt(), readTree())
            case LAMBDAtpt =>
              val tparams = readParams[TypeDef](TYPEPARAM)
              val body = readTpt()
              LambdaTypeTree(tparams, body)
            case MATCHtpt =>
              val fst = readTpt()
              val (bound, scrut) =
                if (nextUnsharedTag == CASEDEF) (EmptyTree, fst) else (fst, readTpt())
              val tpt = MatchTypeTree(bound, scrut, readCases(end))
              // If a match type definition can reduce (e.g. Id in i18261.min)
              // then it's important to trigger that reduction
              // before a TypeVar is added to the constraint,
              // associated to the match type's type parameter.
              // Otherwise, if the reduction is triggered with that constraint,
              // the reduction will be simplified,
              // at which point the TypeVar will replace the type parameter
              // and then that TypeVar will be cached
              // as the reduction of the match type definition!
              //
              // We also override the type, as that's what Typer does.
              tpt.overwriteType(tpt.tpe.normalized)
              tpt
            case TYPEBOUNDStpt =>
              val lo = readTpt()
              val hi = if currentAddr == end then lo else readTpt()
              val alias = if currentAddr == end then EmptyTree else readTpt()
              createNullableTypeBoundsTree(lo, hi, alias)
            case HOLE =>
              readHole(end, isTerm = true)
            case _ =>
              readPathTree()
          }
        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      val tree = if (tag < firstLengthTreeTag) readSimpleTree() else readLengthTree()
      setSpan(start, tree)
    }

    def readTpt()(using Context): Tree = {
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return readTpt()(using sctx)
      val start = currentAddr
      val tree = nextByte match {
        case SHAREDterm =>
          readByte()
          forkAt(readAddr()).readTpt()
        case BLOCK =>
          readByte()
          val end = readEnd()
          val typeReader = fork
          skipTree()
          val aliases = readStats(ctx.owner, end)
          val tpt = typeReader.readTpt()
          Block(aliases, tpt)
        case HOLE =>
          readByte()
          val end = readEnd()
          readHole(end, isTerm = false)
        case _ =>
          if (isTypeTreeTag(nextByte)) readTree()
          else {
            val start = currentAddr
            val tp = readType()
            if (tp.exists) setSpan(start, TypeTree(tp)) else EmptyTree
          }
      }
      setSpan(start, tree)
    }

    def readCases(end: Addr)(using Context): List[CaseDef] =
      collectWhile((nextUnsharedTag == CASEDEF) && currentAddr != end) {
        if (nextByte == SHAREDterm) {
          readByte()
          forkAt(readAddr()).readCase()(using ctx.fresh.setNewScope)
        }
        else readCase()(using ctx.fresh.setNewScope)
      }

    def readCase()(using Context): CaseDef = {
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return readCase()(using sctx)
      val start = currentAddr
      assert(readByte() == CASEDEF)
      val end = readEnd()
      val pat = readTree()
      val rhs = readTree()
      val guard = ifBefore(end)(readTree(), EmptyTree)
      setSpan(start, CaseDef(pat, guard, rhs))
    }

    def readHole(end: Addr, isTerm: Boolean)(using Context): Tree =
      val idx = readNat()
      val tpe = readType()
      val args = until(end)(readTree())
      Hole(isTerm, idx, args, EmptyTree, tpe)

    def readLater[T <: AnyRef](end: Addr, op: TreeReader => Context ?=> T)(using Context): Trees.Lazy[T] =
      readLaterWithOwner(end, op)(ctx.owner)

    def readLaterWithOwner[T <: AnyRef](end: Addr, op: TreeReader => Context ?=> T)(using Context): Symbol => Trees.Lazy[T] = {
      val localReader = fork
      goto(end)
      val mode = ctx.mode
      val source = ctx.source
      owner => new LazyReader(localReader, owner, mode, source, op)
    }

// ------ Setting positions ------------------------------------------------

    /** Pickled span for `addr`. */
    def spanAt(addr: Addr)(using Context): Span =
      if (ctx.mode.is(Mode.ReadPositions))
        posUnpicklerOpt match {
          case Some(posUnpickler) =>
            posUnpickler.spanAt(addr)
          case _  =>
            NoSpan
        }
      else NoSpan

    /** Coordinate for the symbol at `addr`. */
    def coordAt(addr: Addr)(using Context): Coord = {
      val span = spanAt(addr)
      if (span.exists)
        spanCoord(span)
      else
        indexCoord(addr.index)
    }

    /** Pickled source path at `addr`. */
    def sourcePathAt(addr: Addr)(using Context): String =
      if (ctx.mode.is(Mode.ReadPositions))
        posUnpicklerOpt match {
          case Some(posUnpickler) =>
            posUnpickler.sourcePathAt(addr)
          case _  =>
            ""
        }
      else ""

    /** If currentAddr carries a source path, the current context with
     *  the source of that path, otherwise the current context itself.
     */
    def sourceChangeContext(addr: Addr = currentAddr)(using Context): Context = {
      val path = sourcePathAt(addr)
      if (path.nonEmpty) {
        val sourceFile = ctx.getSource(path)
        posUnpicklerOpt match
          case Some(posUnpickler) if !sourceFile.initialized =>
            sourceFile.setLineIndicesFromLineSizes(posUnpickler.lineSizes)
          case _ =>
        pickling.println(i"source change at $addr: $path")
        ctx.withSource(sourceFile)
      }
      else ctx
    }

    /** Set position of `tree` at given `addr`. */
    def setSpan[T <: untpd.Tree](addr: Addr, tree: T)(using Context): tree.type = {
      val span = spanAt(addr)
      if (span.exists) tree.span = span
      tree
    }
  }

  class LazyReader[T <: AnyRef](
      reader: TreeReader, owner: Symbol, mode: Mode, source: SourceFile,
      op: TreeReader => Context ?=> T) extends Trees.Lazy[T] {
    def complete(using Context): T = {
      pickling.println(i"starting to read at ${reader.reader.currentAddr} with owner $owner")
      atPhaseBeforeTransforms {
        op(reader)(using ctx
          .withOwner(owner)
          .withModeBits(mode)
          .withSource(source))
      }
    }
  }

  /** A lazy datastructure that records how definitions are nested in TASTY data.
   *  The structure is lazy because it needs to be computed only for forward references
   *  to symbols that happen before the referenced symbol is created (see `symbolAt`).
   *  Such forward references are rare.
   *
   *  @param   addr    The address of tree representing an owning definition, NoAddr for root tree
   *  @param   tag     The tag at `addr`. Used to determine which subtrees to scan for children
   *                   (i.e. if `tag` is template, don't scan member defs, as these belong already
   *                    to enclosing class).
   *  @param   reader  The reader to be used for scanning for children
   *  @param   end     The end of the owning definition
   */
  class OwnerTree(val addr: Addr, tag: Int, reader: TreeReader, val end: Addr) {

    private var myChildren: List[OwnerTree] = null

    /** All definitions that have the definition at `addr` as closest enclosing definition */
    def children: List[OwnerTree] = {
      if (myChildren == null) myChildren = {
        val buf = new ListBuffer[OwnerTree]
        reader.scanTrees(buf, end, if (tag == TEMPLATE) NoMemberDefs else AllDefs)
        buf.toList
      }
      myChildren
    }

    /** Find the owner of definition at `addr` */
    def findOwner(addr: Addr)(using Context): Symbol = {
      def search(cs: List[OwnerTree], current: Symbol): Symbol =
        try cs match {
          case ot :: cs1 =>
            if (ot.addr.index == addr.index) {
              assert(current.exists, i"no symbol at $addr")
              current
            }
            else if (ot.addr.index < addr.index && addr.index < ot.end.index)
              search(ot.children, reader.symbolAt(ot.addr))
            else
              search(cs1, current)
          case Nil =>
            throw new TreeWithoutOwner
        }
        catch {
          case ex: TreeWithoutOwner =>
            pickling.println(i"no owner for $addr among $cs%, %")
            throw ex
        }
      try search(children, rootOwner)
      catch {
        case ex: TreeWithoutOwner =>
          pickling.println(s"ownerTree = $ownerTree")
          throw ex
      }
    }

    override def toString: String =
      s"OwnerTree(${addr.index}, ${end.index}, ${if (myChildren == null) "?" else myChildren.mkString(" ")})"
  }
}

object TreeUnpickler {

  /** Define the expected format of the tasty bytes
   *   - TopLevel: Tasty that contains a full class nested in its package
   *   - Term: Tasty that contains only a term tree
   *   - TypeTree: Tasty that contains only a type tree or a reference to a type
   */
  sealed trait UnpickleMode
  object UnpickleMode {
    /** Unpickle a full class in some package */
    object TopLevel extends UnpickleMode
    /** Unpickle as a TermTree */
    object Term extends UnpickleMode
    /** Unpickle as a TypeTree */
    object TypeTree extends UnpickleMode
  }

  /** A marker value used to detect cyclic reference while unpickling definitions. */
  @sharable val PoisonTree: tpd.Tree = new EmptyTree

  /** An enumeration indicating which subtrees should be added to an OwnerTree. */
  type MemberDefMode = Int
  inline val MemberDefsOnly = 0   // add only member defs; skip other statements
  inline val NoMemberDefs = 1     // add only statements that are not member defs
  inline val AllDefs = 2          // add everything

  class TreeWithoutOwner extends Exception

  /** An attachment key indicating that an old-style leading () in a constructor
   *  call that is followed by a using clause was suppressed.
   */
  val SuppressedApplyToNone: Property.Key[Unit] = Property.Key()

  /** An attachment key indicating that a trailing () in a constructor
   *  call that has otherwise only using clauses was inserted.
   */
  val InsertedApplyToNone: Property.Key[Unit] = Property.Key()
}
