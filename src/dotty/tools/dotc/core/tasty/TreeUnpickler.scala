package dotty.tools
package dotc
package core
package tasty

import Contexts._, Symbols._, Types._, Scopes._, SymDenotations._, Names._, NameOps._
import StdNames._, Denotations._, Flags._, Constants._, Annotations._
import util.Positions._
import ast.{tpd, Trees, untpd}
import Trees._
import Decorators._
import TastyUnpickler._, TastyBuffer._, PositionPickler._
import scala.annotation.{tailrec, switch}
import scala.collection.mutable.ListBuffer
import scala.collection.{ mutable, immutable }
import config.Printers.pickling

/** Unpickler for typed trees
 *  @param reader         the reader from which to unpickle
 *  @param tastyName      the nametable
 */
class TreeUnpickler(reader: TastyReader, tastyName: TastyName.Table) {
  import TastyFormat._
  import TastyName._
  import TreeUnpickler._
  import tpd._

  private var readPositions = false
  private var totalRange = NoPosition
  private var positions: collection.Map[Addr, Position] = _

  /** Make a subsequent call to `unpickle` return trees with positions
   *  @param totalRange     the range position enclosing all returned trees,
   *                        or NoPosition if positions should not be unpickled
   *  @param positions      a map from tree addresses to their positions relative
   *                        to positions of parent nodes.
   */
  def usePositions(totalRange: Position, positions: collection.Map[Addr, Position]): Unit = {
    readPositions = true
    this.totalRange = totalRange
    this.positions = positions
  }

  /** A map from addresses of definition entries to the symbols they define */
  private val symAtAddr  = new mutable.HashMap[Addr, Symbol]

  /** A temporary map from addresses of definition entries to the trees they define.
   *  Used to remember trees of symbols that are created by a completion. Emptied
   *  once the tree is inlined into a larger tree.
   */
  private val treeAtAddr = new mutable.HashMap[Addr, Tree]

  /** A map from addresses of type entries to the types they define.
   *  Currently only populated for types that might be recursively referenced
   *  from within themselves (i.e. RefinedTypes, PolyTypes, MethodTypes).
   */
  private val typeAtAddr = new mutable.HashMap[Addr, Type]

  /** The root symbol denotation which are defined by the Tasty file associated with this
   *  TreeUnpickler. Set by `enterTopLevel`.
   */
  private var roots: Set[SymDenotation] = null

  /** The root symbols that are defined in this Tasty file. This
   *  is a subset of `roots.map(_.symbol)`.
   */
  private var seenRoots: Set[Symbol] = Set()

  /** The root owner tree. See `OwnerTree` class definition. Set by `enterTopLevel`. */
  private var ownerTree: OwnerTree = _

  private def registerSym(addr: Addr, sym: Symbol) =
    symAtAddr(addr) = sym

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enterTopLevel(roots: Set[SymDenotation])(implicit ctx: Context): Unit = {
    this.roots = roots
    var rdr = new TreeReader(reader).fork
    ownerTree = new OwnerTree(NoAddr, 0, rdr.fork, reader.endAddr)
    rdr.indexStats(reader.endAddr)
  }

  /** The unpickled trees */
  def unpickle()(implicit ctx: Context): List[Tree] = {
    assert(roots != null, "unpickle without previous enterTopLevel")
    val stats = new TreeReader(reader)
      .readTopLevel()(ctx.addMode(Mode.AllowDependentFunctions))
    normalizePos(stats, totalRange)
    stats
  }

  def toTermName(tname: TastyName): TermName = tname match {
    case Simple(name) => name
    case Qualified(qual, name) => toTermName(qual) ++ "." ++ toTermName(name)
    case Signed(original, params, result) => toTermName(original)
    case Shadowed(original) => toTermName(original).shadowedName
    case Expanded(prefix, original) => toTermName(original).expandedName(toTermName(prefix))
    case ModuleClass(original) => toTermName(original).moduleClassName.toTermName
    case SuperAccessor(accessed) => toTermName(accessed).superName
    case DefaultGetter(meth, num) => ???
  }

  def toTermName(ref: NameRef): TermName = toTermName(tastyName(ref))
  def toTypeName(ref: NameRef): TypeName = toTermName(ref).toTypeName

  class Completer(owner: Symbol, reader: TastyReader) extends LazyType {
    import reader._
    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
      treeAtAddr(currentAddr) =
        new TreeReader(reader).readIndexedDef()(
          ctx.withPhaseNoLater(ctx.picklerPhase).withOwner(owner))
    }
  }

  class TreeReader(val reader: TastyReader) {
    import reader._

    def forkAt(start: Addr) = new TreeReader(subReader(start, endAddr))
    def fork = forkAt(currentAddr)

    def skipTree(tag: Int): Unit =
      if (tag >= firstLengthTreeTag) goto(readEnd())
      else if (tag >= firstNatASTTreeTag) { readNat(); skipTree() }
      else if (tag >= firstASTTreeTag) skipTree()
      else if (tag >= firstNatTreeTag) readNat()
    def skipTree(): Unit = skipTree(readByte())

    def skipParams(): Unit =
      while (nextByte == PARAMS || nextByte == TYPEPARAM) skipTree()

    /** Record all directly nested definitions and templates in current tree
     *  as `OwnerTree`s in `buf`
     */
    def scanTree(buf: ListBuffer[OwnerTree], mode: MemberDefMode = AllDefs): Unit = {
      val start = currentAddr
      val tag = readByte()
      tag match {
        case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | TEMPLATE =>
          val end = readEnd()
          for (i <- 0 until numRefs(tag)) readNat()
          if (tag == TEMPLATE) scanTrees(buf, end, MemberDefsOnly)
          if (mode != NoMemberDefs) buf += new OwnerTree(start, tag, fork, end)
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
      if (tag == SHARED) {
        val lookAhead = fork
        lookAhead.reader.readByte()
        forkAt(lookAhead.reader.readAddr()).nextUnsharedTag
      }
      else tag
    }

    def readName(): TermName = toTermName(readNameRef())

    def readNameSplitSig()(implicit ctx: Context): Any /* TermName | (TermName, Signature) */ =
      tastyName(readNameRef()) match {
        case Signed(original, params, result) =>
          var sig = Signature(params map toTypeName, toTypeName(result))
          if (sig == Signature.NotAMethod) sig = Signature.NotAMethod
          (toTermName(original), sig)
        case name =>
          toTermName(name)
      }

// ------ Reading types -----------------------------------------------------

    /** Read names in an interleaved sequence of (parameter) names and types/bounds */
    def readParamNames[N <: Name](end: Addr): List[N] =
      until(end) {
        val name = readName().asInstanceOf[N]
        skipTree()
        name
      }

    /** Read types or bounds in an interleaved sequence of (parameter) names and types/bounds */
    def readParamTypes[T <: Type](end: Addr)(implicit ctx: Context): List[T] =
      until(end) { readNat(); readType().asInstanceOf[T] }

    /** Read referece to definition and return symbol created at that definition */
    def readSymRef()(implicit ctx: Context): Symbol = symbolAt(readAddr())

    /** The symbol at given address; createa new one if none exists yet */
    def symbolAt(addr: Addr)(implicit ctx: Context): Symbol = symAtAddr.get(addr) match {
      case Some(sym) =>
        sym
      case None =>
        val sym = forkAt(addr).createSymbol()(ctx.withOwner(ownerTree.findOwner(addr)))
        ctx.log(i"forward reference to $sym")
        sym
    }

    /** The symbol defined by current definition */
    def symbolAtCurrent()(implicit ctx: Context): Symbol = symAtAddr.get(currentAddr) match {
      case Some(sym) =>
        assert(ctx.owner == sym.owner, i"owner discrepancy for $sym, expected: ${ctx.owner}, found: ${sym.owner}")
        sym
      case None =>
        createSymbol()
    }

    /** Read a type */
    def readType()(implicit ctx: Context): Type = {
      val start = currentAddr
      val tag = readByte()
      pickling.println(s"reading type ${astTagToString(tag)} at $start")

      def registeringType[T](tp: Type, op: => T): T = {
        typeAtAddr(start) = tp
        op
      }

      def readLengthType(): Type = {
        val end = readEnd()

        def readNamesSkipParams[N <: Name]: (List[N], TreeReader) = {
          val nameReader = fork
          nameReader.skipTree() // skip result
          val paramReader = nameReader.fork
          (nameReader.readParamNames[N](end), paramReader)
        }

        val result =
          (tag: @switch) match {
            case SUPERtype =>
              SuperType(readType(), readType())
            case REFINEDtype =>
              var name: Name = readName()
              val parent = readType()
              val ttag = nextUnsharedTag
              if (ttag == TYPEBOUNDS || ttag == TYPEALIAS) name = name.toTypeName
              RefinedType(parent, name, rt => registeringType(rt, readType()))
                // Note that the lambda "rt => ..." is not equivalent to a wildcard closure!
                // Eta expansion of the latter puts readType() out of the expression.
            case APPLIEDtype =>
              readType().appliedTo(until(end)(readType()))
            case TYPEBOUNDS =>
              val lo = readType()
              val hi = readType()
              val bk = ifBefore(end)(new BindingKind(readNat().toByte), NoBinding)
              TypeBounds(lo, hi, bk)
            case TYPEALIAS =>
              val alias = readType()
              val variance =
                if (nextByte == COVARIANT) { readByte(); 1 }
                else if (nextByte == CONTRAVARIANT) { readByte(); -1 }
                else 0
              TypeAlias(alias, variance)
            case ANNOTATED =>
              AnnotatedType(readType(), Annotation(readTerm()))
            case ANDtype =>
              AndType(readType(), readType())
            case ORtype =>
              OrType(readType(), readType())
            case BIND =>
              val sym = ctx.newSymbol(ctx.owner, readName().toTypeName, BindDefinedType, readType())
              registerSym(start, sym)
              TypeRef.withFixedSym(NoPrefix, sym.name, sym)
            case POLYtype =>
              val (names, paramReader) = readNamesSkipParams[TypeName]
              val result = PolyType(names)(
                pt => registeringType(pt, paramReader.readParamTypes[TypeBounds](end)),
                pt => readType())
              goto(end)
              result
            case METHODtype =>
              val (names, paramReader) = readNamesSkipParams[TermName]
              val result = MethodType(names, paramReader.readParamTypes[Type](end))(
                mt => registeringType(mt, readType()))
              goto(end)
              result
            case PARAMtype =>
              readTypeRef() match {
                case binder: PolyType => PolyParam(binder, readNat())
                case binder: MethodType => MethodParam(binder, readNat())
              }
            case CLASSconst =>
              ConstantType(Constant(readType()))
            case ENUMconst =>
              ConstantType(Constant(readTermRef().termSymbol))
          }
        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      def readSimpleType(): Type = (tag: @switch) match {
        case TYPEREFdirect | TERMREFdirect =>
          NamedType.withFixedSym(NoPrefix, readSymRef())
        case TYPEREFsymbol | TERMREFsymbol =>
          readSymNameRef()
        case TYPEREFpkg =>
          readPackageRef().moduleClass.typeRef
        case TERMREFpkg =>
          readPackageRef().termRef
        case TYPEREF =>
          val name =  readName().toTypeName
          if (name.isLambdaTraitName) // Make sure curresponding lambda trait exists
            defn.LambdaTrait(name.lambdaTraitVariances)
          TypeRef(readType(), name)
        case TERMREF =>
          readNameSplitSig() match {
            case name: TermName => TermRef.all(readType(), name)
            case (name: TermName, sig: Signature) => TermRef.withSig(readType(), name, sig)
          }
        case THIS =>
          ThisType.raw(readType().asInstanceOf[TypeRef])
        case REFINEDthis =>
          RefinedThis(readTypeRef().asInstanceOf[RefinedType])
        case SHARED =>
          val ref = readAddr()
          typeAtAddr.getOrElseUpdate(ref, forkAt(ref).readType())
        case UNITconst =>
          ConstantType(Constant(()))
        case TRUEconst =>
          ConstantType(Constant(true))
        case FALSEconst =>
          ConstantType(Constant(false))
        case BYTEconst =>
          ConstantType(Constant(readInt().toByte))
        case SHORTconst =>
          ConstantType(Constant(readInt().toShort))
        case CHARconst =>
          ConstantType(Constant(readNat().toChar))
        case INTconst =>
          ConstantType(Constant(readInt()))
        case LONGconst =>
          ConstantType(Constant(readLongInt()))
        case FLOATconst =>
          ConstantType(Constant(java.lang.Float.intBitsToFloat(readInt())))
        case DOUBLEconst =>
          ConstantType(Constant(java.lang.Double.longBitsToDouble(readLongInt())))
        case STRINGconst =>
          ConstantType(Constant(readName().toString))
        case NULLconst =>
          ConstantType(Constant(null))
        case CLASSconst =>
          ConstantType(Constant(readType()))
        case BYNAMEtype =>
          ExprType(readType())
      }

      if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
    }

    private def readSymNameRef()(implicit ctx: Context): Type = {
      val sym = readSymRef()
      val prefix = readType()
      val res = NamedType.withSymAndName(prefix, sym, sym.name)
      prefix match {
        case prefix: ThisType if prefix.cls eq sym.owner => res.withDenot(sym.denot)
          // without this precaution we get an infinite cycle when unpickling pos/extmethods.scala
          // the problem arises when a self type of a trait is a type parameter of the same trait.
        case _ => res
      }
    }

    private def readPackageRef()(implicit ctx: Context): TermSymbol = {
      val name = readName()
      if (name == nme.ROOT) defn.RootPackage
      else if (name == nme.EMPTY_PACKAGE) defn.EmptyPackageVal
      else ctx.requiredPackage(name)
    }

    def readTypeRef(): Type =
      typeAtAddr(readAddr())

    def readPath()(implicit ctx: Context): Type = {
      val tp = readType()
      assert(tp.isInstanceOf[SingletonType])
      tp
    }

    def readTermRef()(implicit ctx: Context): TermRef =
      readType().asInstanceOf[TermRef]

// ------ Reading definitions -----------------------------------------------------

    private def noRhs(end: Addr): Boolean =
      currentAddr == end || isModifierTag(nextByte)

    private def localContext(owner: Symbol)(implicit ctx: Context) = {
      val lctx = ctx.fresh.setOwner(owner)
      if (owner.isClass) lctx.setScope(owner.unforcedDecls) else lctx.setNewScope
    }

    private def normalizeFlags(tag: Int, givenFlags: FlagSet, name: Name, isAbstractType: Boolean, rhsIsEmpty: Boolean)(implicit ctx: Context): FlagSet = {
      val lacksDefinition =
        rhsIsEmpty &&
          name.isTermName && !name.isConstructorName && !givenFlags.is(ParamOrAccessor) ||
        isAbstractType
      var flags = givenFlags
      if (lacksDefinition && tag != PARAM) flags |= Deferred
      if (tag == DEFDEF) flags |= Method
      if (givenFlags is Module)
        flags = flags | (if (tag == VALDEF) ModuleCreationFlags else ModuleClassCreationFlags)
      if (ctx.owner.isClass) {
        if (tag == TYPEPARAM) flags |= Param
        else if (tag == PARAM) flags |= ParamAccessor
      }
      else if (isParamTag(tag)) flags |= Param
      flags
    }

    /** Create symbol of definition node and enter in symAtAddr map
     *  @return  the created symbol
     */
    def createSymbol()(implicit ctx: Context): Symbol = nextByte match {
      case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
        createMemberSymbol()
      case TEMPLATE =>
        val localDummy = ctx.newLocalDummy(ctx.owner)
        registerSym(currentAddr, localDummy)
        localDummy
      case tag =>
        throw new Error(s"illegal createSymbol at $currentAddr, tag = $tag")
    }

    /** Create symbol of member definition or parameter node and enter in symAtAddr map
     *  @return  the created symbol
     */
    def createMemberSymbol()(implicit ctx: Context): Symbol = {
      val start = currentAddr
      val tag = readByte()
      val end = readEnd()
      val rawName = tastyName(readNameRef())
      var name: Name = toTermName(rawName)
      if (tag == TYPEDEF || tag == TYPEPARAM) name = name.toTypeName
      skipParams()
      val ttag = nextUnsharedTag
      val isAbstractType = ttag == TYPEBOUNDS
      val isClass = ttag == TEMPLATE
      val templateStart = currentAddr
      skipTree() // tpt
      val rhsIsEmpty = noRhs(end)
      if (!rhsIsEmpty) skipTree()
      val (givenFlags, annots, privateWithin) = readModifiers(end)
      def nameFlags(tname: TastyName): FlagSet = tname match {
        case TastyName.Expanded(_, original) => ExpandedName | nameFlags(tastyName(original))
        case TastyName.SuperAccessor(_) => Flags.SuperAccessor
        case _ => EmptyFlags
      }
      pickling.println(i"creating symbol $name at $start with flags $givenFlags")
      val flags = normalizeFlags(tag, givenFlags | nameFlags(rawName), name, isAbstractType, rhsIsEmpty)
      def adjustIfModule(completer: LazyType) =
        if (flags is Module) ctx.adjustModuleCompleter(completer, name) else completer
      val sym =
        roots.find(root => (root.owner eq ctx.owner) && root.name == name) match {
          case Some(rootd) =>
            pickling.println(i"overwriting ${rootd.symbol} # ${rootd.hashCode}")
            rootd.info = adjustIfModule(
                new Completer(ctx.owner, subReader(start, end)) with SymbolLoaders.SecondCompleter)
            rootd.flags = flags &~ Touched // allow one more completion
            rootd.privateWithin = privateWithin
            seenRoots += rootd.symbol
            rootd.symbol
          case _ =>
            val completer = adjustIfModule(new Completer(ctx.owner, subReader(start, end)))
            if (isClass)
              ctx.newClassSymbol(ctx.owner, name.asTypeName, flags, completer, privateWithin, coord = start.index)
            else
              ctx.newSymbol(ctx.owner, name, flags, completer, privateWithin, coord = start.index)
        } // TODO set position somehow (but take care not to upset Symbol#isDefinedInCurrentRun)
      sym.annotations = annots
      ctx.enter(sym)
      registerSym(start, sym)
      if (isClass) {
        sym.completer.withDecls(newScope)
        forkAt(templateStart).indexTemplateParams()(localContext(sym))
      }
      goto(start)
      sym
    }

    /** Read modifier list into triplet of flags, annotations and a privateWithin
     *  boundary symbol.
     */
    def readModifiers(end: Addr)(implicit ctx: Context): (FlagSet, List[Annotation], Symbol) = {
      var flags: FlagSet = EmptyFlags
      var annots = new mutable.ListBuffer[Annotation]
      var privateWithin: Symbol = NoSymbol
      while (currentAddr.index != end.index) {
        def addFlag(flag: FlagSet) = {
          flags |= flag
          readByte()
        }
        nextByte match {
          case PRIVATE => addFlag(Private)
          case INTERNAL => ??? // addFlag(Internal)
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
          case LAZY => addFlag(Lazy)
          case OVERRIDE => addFlag(Override)
          case INLINE => addFlag(Inline)
          case STATIC => addFlag(JavaStatic)
          case OBJECT => addFlag(Module)
          case TRAIT => addFlag(Trait)
          case LOCAL => addFlag(Local)
          case SYNTHETIC => addFlag(Synthetic)
          case ARTIFACT => addFlag(Artifact)
          case MUTABLE => addFlag(Mutable)
          case LABEL => addFlag(Label)
          case FIELDaccessor => addFlag(Accessor)
          case CASEaccessor => addFlag(CaseAccessor)
          case COVARIANT => addFlag(Covariant)
          case CONTRAVARIANT => addFlag(Contravariant)
          case SCALA2X => addFlag(Scala2x)
          case DEFAULTparameterized => addFlag(DefaultParameterized)
          case INSUPERCALL => addFlag(InSuperCall)
          case STABLE => addFlag(Stable)
          case PRIVATEqualified =>
            readByte()
            privateWithin = readType().typeSymbol
          case PROTECTEDqualified =>
            addFlag(Protected)
            privateWithin = readType().typeSymbol
          case ANNOTATION =>
            readByte()
            val end = readEnd()
            val sym = readType().typeSymbol
            val lazyAnnotTree = readLater(end, rdr => ctx => rdr.readTerm()(ctx))
            annots += Annotation.deferred(sym, _ => lazyAnnotTree.complete)
          case _ =>
            assert(false, s"illegal modifier tag at $currentAddr")
        }
      }
      (flags, annots.toList, privateWithin)
    }

    /** Create symbols for the definitions in the statement sequence between
     *  current address and `end`.
     *  @return  the largest subset of {NoInits, PureInterface} that a
     *           trait owning the indexed statements can have as flags.
     */
    def indexStats(end: Addr)(implicit ctx: Context): FlagSet = {
      var initsFlags = NoInitsInterface
      while (currentAddr.index < end.index) {
        nextByte match {
          case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
            val sym = symbolAtCurrent()
            skipTree()
            if (sym.isTerm && !sym.is(MethodOrLazyOrDeferred))
              initsFlags = EmptyFlags
            else if (sym.isClass ||
              sym.is(Method, butNot = Deferred) && !sym.isConstructor)
              initsFlags &= NoInits
          case IMPORT =>
            skipTree()
          case PACKAGE =>
            processPackage { (pid, end) => implicit ctx => indexStats(end) }
          case _ =>
            skipTree()
            initsFlags = EmptyFlags
        }
      }
      assert(currentAddr.index == end.index)
      initsFlags
    }

    /** Process package with given operation `op`. The operation takes as arguments
     *   - a `RefTree` representing the `pid` of the package,
     *   - an end address,
     *   - a context which has the processd package as owner
     */
    def processPackage[T](op: (RefTree, Addr) => Context => T)(implicit ctx: Context): T = {
      readByte()
      val end = readEnd()
      val pid = ref(readTermRef()).asInstanceOf[RefTree]
      op(pid, end)(localContext(pid.symbol.moduleClass))
    }

    /** Create symbols the longest consecutive sequence of parameters with given
     *  `tag` starting at current address.
     */
    def indexParams(tag: Int)(implicit ctx: Context) =
      while (nextByte == tag) {
        symbolAtCurrent()
        skipTree()
      }

    /** Create symbols for all type and value parameters of template starting
     *  at current address.
     */
    def indexTemplateParams()(implicit ctx: Context) = {
      assert(readByte() == TEMPLATE)
      readEnd()
      indexParams(TYPEPARAM)
      indexParams(PARAM)
    }

    /** If definition was already read by a completer, return the previously read tree
     *  or else read definition.
     */
    def readIndexedDef()(implicit ctx: Context): Tree = treeAtAddr.remove(currentAddr) match {
      case Some(tree) => skipTree(); tree
      case none => readNewDef()
    }

    private def readNewDef()(implicit ctx: Context): Tree = {
      val start = currentAddr
      val sym = symAtAddr(start)
      val tag = readByte()
      val end = readEnd()

      def readParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] = {
        fork.indexParams(tag)(localContext(sym))
        readIndexedParams(tag)
      }

      def readParamss(implicit ctx: Context): List[List[ValDef]] = {
        collectWhile(nextByte == PARAMS) {
          readByte()
          readEnd()
          readParams[ValDef](PARAM)
        }
      }

      def readRhs(implicit ctx: Context) =
        if (noRhs(end)) EmptyTree
        else readLater(end, rdr => ctx => rdr.readTerm()(ctx))

      def localCtx = localContext(sym)

      def DefDef(tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree) =
         ta.assignType(
            untpd.DefDef(
              sym.name.asTermName, tparams, vparamss, tpt, readRhs(localCtx)),
            sym)

      def ta =  ctx.typeAssigner

      val name = readName()
      pickling.println(s"reading def of $name at $start")
      val tree: MemberDef = tag match {
        case DEFDEF =>
          val tparams = readParams[TypeDef](TYPEPARAM)(localCtx)
          val vparamss = readParamss(localCtx)
          val tpt = readTpt()
          val typeParams = tparams.map(_.symbol)
          val valueParamss = ctx.normalizeIfConstructor(
              vparamss.nestedMap(_.symbol), name == nme.CONSTRUCTOR)
          val resType = ctx.effectiveResultType(sym, typeParams, tpt.tpe)
          sym.info = ctx.methodType(typeParams, valueParamss, resType)
          if (sym.isSetter && sym.accessedFieldOrGetter.is(ParamAccessor)) {
            // reconstitute ParamAccessor flag of setters for var parameters, which is not pickled
            sym.setFlag(ParamAccessor)
            sym.resetFlag(Deferred)
          }
          DefDef(tparams, vparamss, tpt)
        case VALDEF =>
          sym.info = readType()
          ValDef(sym.asTerm, readRhs(localCtx))
        case TYPEDEF | TYPEPARAM =>
          if (sym.isClass) {
            val companion = sym.scalacLinkedClass

            // Is the companion defined in the same Tasty file as `sym`?
            // The only case to check here is if `sym` is a root. In this case
            // `companion` might have been entered by the environment but it might
            // be missing from the Tasty file. So we check explicitly for that.
            def isCodefined =
              roots.contains(companion.denot) == seenRoots.contains(companion)
            if (companion.exists && isCodefined) {
              import transform.SymUtils._
              if (sym is Flags.ModuleClass) sym.registerCompanionMethod(nme.COMPANION_CLASS_METHOD, companion)
              else sym.registerCompanionMethod(nme.COMPANION_MODULE_METHOD, companion)
            }
            ta.assignType(untpd.TypeDef(sym.name.asTypeName, readTemplate(localCtx)), sym)
          } else {
            sym.info = readType()
            TypeDef(sym.asType)
          }
        case PARAM =>
          val info = readType()
          if (noRhs(end)) {
            sym.info = info
            ValDef(sym.asTerm)
          }
          else {
            sym.setFlag(Method)
            sym.info = ExprType(info)
            pickling.println(i"reading param alias $name -> $currentAddr")
            DefDef(Nil, Nil, TypeTree(info))
          }
      }
      val mods =
        if (sym.annotations.isEmpty) EmptyModifiers
        else Modifiers(annotations = sym.annotations.map(_.tree))
      tree.withMods(mods) // record annotations in tree so that tree positions can be filled in.
      goto(end)
      setPos(start, tree)
    }

    private def readTemplate(implicit ctx: Context): Template = {
      val start = currentAddr
      val cls = ctx.owner.asClass
      def setClsInfo(parents: List[TypeRef], selfType: Type) =
        cls.info = ClassInfo(cls.owner.thisType, cls, parents, cls.unforcedDecls, selfType)
      val assumedSelfType =
        if (cls.is(Module) && cls.owner.isClass)
          TermRef.withSig(cls.owner.thisType, cls.name.sourceModuleName, Signature.NotAMethod)
        else NoType
      setClsInfo(Nil, assumedSelfType)
      val localDummy = symbolAtCurrent()
      assert(readByte() == TEMPLATE)
      val end = readEnd()
      val tparams = readIndexedParams[TypeDef](TYPEPARAM)
      val vparams = readIndexedParams[ValDef](PARAM)
      val parents = collectWhile(nextByte != SELFDEF && nextByte != DEFDEF) {
        nextByte match {
          case APPLY | TYPEAPPLY => readTerm()
          case _ => readTpt()
        }
      }
      val parentRefs = ctx.normalizeToClassRefs(parents.map(_.tpe), cls, cls.unforcedDecls)
      val self =
        if (nextByte == SELFDEF) {
          readByte()
          untpd.ValDef(readName(), readTpt(), EmptyTree).withType(NoType)
        }
        else EmptyValDef
      setClsInfo(parentRefs, if (self.isEmpty) NoType else self.tpt.tpe)
      cls.setApplicableFlags(fork.indexStats(end))
      val constr = readIndexedDef().asInstanceOf[DefDef]

      def mergeTypeParamsAndAliases(tparams: List[TypeDef], stats: List[Tree]): (List[Tree], List[Tree]) =
        (tparams, stats) match {
          case (tparam :: tparams1, (alias: TypeDef) :: stats1)
          if tparam.name == alias.name.expandedName(cls) =>
            val (tas, stats2) = mergeTypeParamsAndAliases(tparams1, stats1)
            (tparam :: alias :: tas, stats2)
          case _ =>
            (tparams, stats)
        }

      val lazyStats = readLater(end, rdr => implicit ctx => {
        val stats0 = rdr.readIndexedStats(localDummy, end)
        val (tparamsAndAliases, stats) = mergeTypeParamsAndAliases(tparams, stats0)
        tparamsAndAliases ++ vparams ++ stats
      })
      setPos(start,
        untpd.Template(constr, parents, self, lazyStats)
          .withType(localDummy.nonMemberTermRef))
    }

    def skipToplevel()(implicit ctx: Context): Unit= {
      if (!isAtEnd)
        nextByte match {
          case IMPORT | PACKAGE =>
            skipTree()
            skipToplevel()
          case _ =>
        }
    }

    def readTopLevel()(implicit ctx: Context): List[Tree] = {
      @tailrec def read(acc: ListBuffer[Tree]): List[Tree] = nextByte match {
        case IMPORT | PACKAGE =>
          acc += readIndexedStat(NoSymbol)
          if (!isAtEnd) read(acc) else acc.toList
        case _ => // top-level trees which are not imports or packages are not part of tree
          acc.toList
      }
      read(new ListBuffer[tpd.Tree])
    }

    def readIndexedStat(exprOwner: Symbol)(implicit ctx: Context): Tree = nextByte match {
      case TYPEDEF | VALDEF | DEFDEF =>
        readIndexedDef()
      case IMPORT =>
        readImport()
      case PACKAGE =>
        val start = currentAddr
        processPackage { (pid, end) => implicit ctx =>
          setPos(start, PackageDef(pid, readIndexedStats(exprOwner, end)(ctx)))
        }
      case _ =>
        readTerm()(ctx.withOwner(exprOwner))
    }

    def readImport()(implicit ctx: Context): Tree = {
      readByte()
      readEnd()
      val expr = readTerm()
      def readSelectors(): List[untpd.Tree] = nextByte match {
        case RENAMED =>
          readByte()
          readEnd()
          untpd.Pair(untpd.Ident(readName()), untpd.Ident(readName())) :: readSelectors()
        case IMPORTED =>
          readByte()
          untpd.Ident(readName()) :: readSelectors()
        case _ =>
          Nil
      }
      Import(expr, readSelectors())
    }

    def readIndexedStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] =
      until(end)(readIndexedStat(exprOwner))

    def readStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] = {
      fork.indexStats(end)
      readIndexedStats(exprOwner, end)
    }

    def readIndexedParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] =
      collectWhile(nextByte == tag) { readIndexedDef().asInstanceOf[T] }

// ------ Reading terms -----------------------------------------------------

    def readTerm()(implicit ctx: Context): Tree = {
      val start = currentAddr
      val tag = readByte()
      pickling.println(s"reading term ${astTagToString(tag)} at $start")

      def readPathTerm(): Tree = {
        goto(start)
        readPath() match {
          case path: TermRef => ref(path)
          case path: ThisType => This(path.cls)
          case path: ConstantType => Literal(path.value)
        }
      }

      def readSimpleTerm(): Tree = tag match {
        case IDENT =>
          untpd.Ident(readName()).withType(readType())
        case SELECT =>
          def readQual(name: Name) = {
            val localCtx =
              if (name == nme.CONSTRUCTOR) ctx.addMode(Mode.InSuperCall) else ctx
            readTerm()(localCtx)
          }
          def readRest(name: Name, sig: Signature) = {
            val unshadowed = if (name.isShadowedName) name.revertShadowed else name
            val qual = readQual(name)
            untpd.Select(qual, unshadowed)
              .withType(TermRef.withSig(qual.tpe.widenIfUnstable, name.asTermName, sig))
          }
          readNameSplitSig match {
            case name: Name => readRest(name, Signature.NotAMethod)
            case (name: Name, sig: Signature) => readRest(name, sig)
          }

        case NEW =>
          New(readTpt())
        case _ =>
          readPathTerm()
      }

      def readLengthTerm(): Tree = {
        val end = readEnd()

        val result =
          (tag: @switch) match {
            case SUPER =>
              val qual = readTerm()
              val mixClass = ifBefore(end)(readType().typeSymbol, NoSymbol)
              val mixName = if (mixClass.exists) mixClass.name.asTypeName else tpnme.EMPTY
              tpd.Super(qual, mixName, ctx.mode.is(Mode.InSuperCall), mixClass)
            case APPLY =>
              val fn = readTerm()
              val isJava = fn.symbol.is(JavaDefined)
              def readArg() = readTerm() match {
                case SeqLiteral(elems, elemtpt) if isJava =>
                  JavaSeqLiteral(elems, elemtpt)
                case arg => arg
              }
              tpd.Apply(fn, until(end)(readArg()))
            case TYPEAPPLY =>
              tpd.TypeApply(readTerm(), until(end)(readTpt()))
            case PAIR =>
              Pair(readTerm(), readTerm())
            case TYPED =>
              val expr = readTerm()
              val tpt = readTpt()
              val expr1 = expr match {
                case SeqLiteral(elems, elemtpt) if tpt.tpe.isRef(defn.ArrayClass) =>
                  JavaSeqLiteral(elems, elemtpt)
                case expr => expr
              }
              Typed(expr1, tpt)
            case NAMEDARG =>
              NamedArg(readName(), readTerm())
            case ASSIGN =>
              Assign(readTerm(), readTerm())
            case BLOCK =>
              val exprReader = fork
              skipTree()
              val localCtx = ctx.fresh.setNewScope
              val stats = readStats(ctx.owner, end)(localCtx)
              val expr = exprReader.readTerm()(localCtx)
              Block(stats, expr)
            case IF =>
              If(readTerm(), readTerm(), readTerm())
            case LAMBDA =>
              val meth = readTerm()
              val tpt = ifBefore(end)(readTpt(), EmptyTree)
              Closure(Nil, meth, tpt)
            case MATCH =>
              Match(readTerm(), readCases(end))
            case RETURN =>
              val from = readSymRef()
              val expr = ifBefore(end)(readTerm(), EmptyTree)
              Return(expr, Ident(from.termRef))
            case TRY =>
              Try(readTerm(), readCases(end), ifBefore(end)(readTerm(), EmptyTree))
            case REPEATED =>
              val elemtpt = readTpt()
              SeqLiteral(until(end)(readTerm()), elemtpt)
            case BIND =>
              val name = readName()
              val info = readType()
              val sym = ctx.newSymbol(ctx.owner, name, EmptyFlags, info)
              registerSym(start, sym)
              Bind(sym, readTerm())
            case ALTERNATIVE =>
              Alternative(until(end)(readTerm()))
            case UNAPPLY =>
              val fn = readTerm()
              val implicitArgs =
                collectWhile(nextByte == IMPLICITarg) {
                  readByte()
                  readTerm()
                }
              val patType = readType()
              val argPats = until(end)(readTerm())
              UnApply(fn, implicitArgs, argPats, patType)
            case _ =>
              readPathTerm()
          }
        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      val tree = if (tag < firstLengthTreeTag) readSimpleTerm() else readLengthTerm()
      tree.overwriteType(tree.tpe.simplified)
      setPos(start, tree)
    }

    def readTpt()(implicit ctx: Context) = {
      val start = currentAddr
      val tp = readType()
      if (tp.exists) setPos(start, TypeTree(tp)) else EmptyTree
    }

    def readCases(end: Addr)(implicit ctx: Context): List[CaseDef] =
      collectWhile(nextByte == CASEDEF && currentAddr != end) { readCase()(ctx.fresh.setNewScope) }

    def readCase()(implicit ctx: Context): CaseDef = {
      val start = currentAddr
      readByte()
      val end = readEnd()
      val pat = readTerm()
      val rhs = readTerm()
      val guard = ifBefore(end)(readTerm(), EmptyTree)
      setPos(start, CaseDef(pat, guard, rhs))
    }

    def readLater[T <: AnyRef](end: Addr, op: TreeReader => Context => T): Trees.Lazy[T] = {
      val localReader = fork
      goto(end)
      new LazyReader(localReader, op)
    }

// ------ Hooks for positions ------------------------------------------------

    /** Record address from which tree was created as a temporary position in the tree.
     *  The temporary position contains deltas relative to the position of the (as yet unknown)
     *  parent node. It is marked as a non-synthetic source position.
     */
    def setPos[T <: Tree](addr: Addr, tree: T): T = {
      if (readPositions)
        tree.setPosUnchecked(positions.getOrElse(addr, Position(0, 0, 0)))
      tree
    }
  }

  private def setNormalized(tree: Tree, parentPos: Position): Unit =
    tree.setPosUnchecked(
      if (tree.pos.exists)
        Position(parentPos.start + offsetToInt(tree.pos.start), parentPos.end - tree.pos.end)
      else
        parentPos)

  def normalizePos(x: Any, parentPos: Position)(implicit ctx: Context): Unit =
    traverse(x, parentPos, setNormalized)

  class LazyReader[T <: AnyRef](reader: TreeReader, op: TreeReader => Context => T) extends Trees.Lazy[T] with DeferredPosition {
    def complete(implicit ctx: Context): T = {
      pickling.println(i"starting to read at ${reader.reader.currentAddr}")
      val res = op(reader)(ctx.addMode(Mode.AllowDependentFunctions).withPhaseNoLater(ctx.picklerPhase))
      normalizePos(res, parentPos)
      res
    }
  }

  class LazyAnnotationReader(sym: Symbol, reader: TreeReader)
      extends LazyAnnotation(sym) with DeferredPosition {
    def complete(implicit ctx: Context) = {
      val res = reader.readTerm()(ctx.withPhaseNoLater(ctx.picklerPhase))
      normalizePos(res, parentPos)
      res
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

    /** All definitions that have the definition at `addr` as closest enclosing definition */
    lazy val children: List[OwnerTree] = {
      val buf = new ListBuffer[OwnerTree]
      reader.scanTrees(buf, end, if (tag == TEMPLATE) NoMemberDefs else AllDefs)
      buf.toList
    }

    /** Find the owner of definition at `addr` */
    def findOwner(addr: Addr)(implicit ctx: Context): Symbol = {
      def search(cs: List[OwnerTree], current: Symbol): Symbol =
        try cs match {
        case ot :: cs1 =>
          if (ot.addr.index == addr.index)
            current
          else if (ot.addr.index < addr.index && addr.index < ot.end.index)
            search(ot.children, reader.symbolAt(ot.addr))
          else
            search(cs1, current)
        case Nil =>
          throw new TreeWithoutOwner
      }
      catch {
        case ex: TreeWithoutOwner =>
          println(i"no owner for $addr among $cs")  // DEBUG
          throw ex
      }
      search(children, NoSymbol)
    }

    override def toString = s"OwnerTree(${addr.index}, ${end.index}"
  }
}

object TreeUnpickler {

  /** An enumeration indicating which subtrees should be added to an OwnerTree. */
  type MemberDefMode = Int
  final val MemberDefsOnly = 0   // add only member defs; skip other statements
  final val NoMemberDefs = 1     // add only statements that are not member defs
  final val AllDefs = 2          // add everything

  class TreeWithoutOwner extends Exception
}


