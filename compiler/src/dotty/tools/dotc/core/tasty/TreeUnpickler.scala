package dotty.tools
package dotc
package core
package tasty

import Contexts._, Symbols._, Types._, Scopes._, SymDenotations._, Names._, NameOps._
import StdNames._, Denotations._, Flags._, Constants._, Annotations._
import NameKinds._
import typer.Checking.checkNonCyclic
import util.Positions._
import ast.{tpd, untpd, Trees}
import Trees._
import Decorators._
import transform.SymUtils._
import TastyUnpickler._, TastyBuffer._
import scala.annotation.{tailrec, switch}
import scala.collection.mutable.ListBuffer
import scala.collection.{ mutable, immutable }
import config.Printers.pickling
import typer.Checking
import config.Config
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.interpreter.RawQuoted
import scala.quoted.Expr

/** Unpickler for typed trees
 *  @param reader          the reader from which to unpickle
 *  @param tastyName       the nametable
 *  @param posUNpicklerOpt the unpickler for positions, if it exists
 */
class TreeUnpickler(reader: TastyReader,
                    nameAtRef: NameRef => TermName,
                    posUnpicklerOpt: Option[PositionUnpickler],
                    splices: Seq[Any]) {
  import TastyFormat._
  import TreeUnpickler._
  import tpd._

  /** A map from addresses of definition entries to the symbols they define */
  private val symAtAddr  = new mutable.HashMap[Addr, Symbol]

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

  /** The root symbol denotation which are defined by the Tasty file associated with this
   *  TreeUnpickler. Set by `enterTopLevel`.
   */
  private[this] var roots: Set[SymDenotation] = null

  /** The root symbols that are defined in this Tasty file. This
   *  is a subset of `roots.map(_.symbol)`.
   */
  private[this] var seenRoots: Set[Symbol] = Set()

  /** The root owner tree. See `OwnerTree` class definition. Set by `enterTopLevel`. */
  private[this] var ownerTree: OwnerTree = _

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
    new TreeReader(reader).readTopLevel()
  }

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
      if (tag == SHAREDtype || tag == SHAREDterm) {
        val lookAhead = fork
        lookAhead.reader.readByte()
        forkAt(lookAhead.reader.readAddr()).nextUnsharedTag
      }
      else tag
    }

    def readName(): TermName = nameAtRef(readNameRef())

// ------ Reading types -----------------------------------------------------

    /** Read names in an interleaved sequence of (parameter) names and types/bounds */
    def readParamNames(end: Addr): List[Name] =
      until(end) {
        val name = readName()
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

        def readMethodic[N <: Name, PInfo <: Type, LT <: LambdaType]
            (companion: LambdaTypeCompanion[N, PInfo, LT], nameMap: Name => N): LT = {
          val result = typeAtAddr.getOrElse(start, {
              val nameReader = fork
              nameReader.skipTree() // skip result
              val paramReader = nameReader.fork
              val paramNames = nameReader.readParamNames(end).map(nameMap)
              companion(paramNames)(
                pt => registeringType(pt, paramReader.readParamTypes[PInfo](end)),
                pt => readType())
            })
          goto(end)
          result.asInstanceOf[LT]
        }

        val result =
          (tag: @switch) match {
            case TERMREFin =>
              var sname = readName()
              val prefix = readType()
              val space = readType()
              sname match {
                case SignedName(name, sig) =>
                  TermRef(prefix, name, space.decl(name).atSignature(sig))
                case name =>
                  TermRef(prefix, name, space.decl(name))
              }
            case TYPEREFin =>
              val name = readName().toTypeName
              val prefix = readType()
              val space = readType()
              space.decl(name) match {
                case symd: SymDenotation if prefix.isArgPrefixOf(symd.symbol) => TypeRef(prefix, symd.symbol)
                case _ => TypeRef(prefix, name, space.decl(name))
              }
            case REFINEDtype =>
              var name: Name = readName()
              val parent = readType()
              val ttag = nextUnsharedTag
              if (ttag == TYPEBOUNDS || ttag == TYPEALIAS) name = name.toTypeName
              RefinedType(parent, name, readType())
                // Note that the lambda "rt => ..." is not equivalent to a wildcard closure!
                // Eta expansion of the latter puts readType() out of the expression.
            case APPLIEDtype =>
              readType().appliedTo(until(end)(readType()))
            case TYPEBOUNDS =>
              TypeBounds(readType(), readType())
            case ANNOTATEDtype =>
              AnnotatedType(readType(), Annotation(readTerm()))
            case ANDtype =>
              AndType(readType(), readType())
            case ORtype =>
              OrType(readType(), readType())
            case SUPERtype =>
              SuperType(readType(), readType())
            case BIND =>
              val sym = ctx.newSymbol(ctx.owner, readName().toTypeName, BindDefinedType, readType(),
                coord = coordAt(start))
              registerSym(start, sym)
              if (currentAddr != end) readType()
              TypeRef(NoPrefix, sym)
            case POLYtype =>
              readMethodic(PolyType, _.toTypeName)
            case METHODtype =>
              readMethodic(MethodType, _.toTermName)
            case TYPELAMBDAtype =>
              readMethodic(HKTypeLambda, _.toTypeName)
            case PARAMtype =>
              readTypeRef() match {
                case binder: LambdaType => binder.paramRefs(readNat())
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
          NamedType(NoPrefix, readSymRef())
        case TYPEREFsymbol | TERMREFsymbol =>
          readSymNameRef()
        case TYPEREFpkg =>
          readPackageRef().moduleClass.typeRef
        case TERMREFpkg =>
          readPackageRef().termRef
        case TYPEREF =>
          val name = readName().toTypeName
          TypeRef(readType(), name)
        case TERMREF =>
          val sname = readName()
          val prefix = readType()
          sname match {
            case SignedName(name, sig) =>
              TermRef(prefix, name, prefix.member(name).atSignature(sig))
            case name =>
              TermRef(prefix, name)
          }
        case THIS =>
          ThisType.raw(readType().asInstanceOf[TypeRef])
        case RECtype =>
          typeAtAddr.getOrElse(start, RecType(rt => registeringType(rt, readType())))
        case RECthis =>
          readTypeRef().asInstanceOf[RecType].recThis
        case TYPEALIAS =>
          TypeAlias(readType())
        case SHAREDtype =>
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
        case ENUMconst =>
          ConstantType(Constant(readTermRef().termSymbol))
        case SYMBOLconst =>
          ConstantType(Constant(scala.Symbol(readName().toString)))
        case BYNAMEtype =>
          ExprType(readType())
      }

      if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
    }

    private def readSymNameRef()(implicit ctx: Context): Type = {
      val sym = readSymRef()
      val prefix = readType()
      val res = NamedType(prefix, sym)
      prefix match {
        case prefix: ThisType if prefix.cls eq sym.owner => res.withDenot(sym.denot)
          // without this precaution we get an infinite cycle when unpickling pos/extmethods.scala
          // the problem arises when a self type of a trait is a type parameter of the same trait.
        case _ => res
      }
    }

    private def readPackageRef()(implicit ctx: Context): TermSymbol = {
      val name = readName()
      if (name == nme.ROOT || name == nme.ROOTPKG) defn.RootPackage
      else if (name == nme.EMPTY_PACKAGE) defn.EmptyPackageVal
      else ctx.requiredPackage(name)
    }

    def readTypeRef(): Type =
      typeAtAddr(readAddr())

    def readTermRef()(implicit ctx: Context): TermRef =
      readType().asInstanceOf[TermRef]

// ------ Reading definitions -----------------------------------------------------

    private def noRhs(end: Addr): Boolean =
      currentAddr == end || isModifierTag(nextByte)

    private def localContext(owner: Symbol)(implicit ctx: Context) =
      ctx.fresh.setOwner(owner)

    private def normalizeFlags(tag: Int, givenFlags: FlagSet, name: Name, isAbsType: Boolean, rhsIsEmpty: Boolean)(implicit ctx: Context): FlagSet = {
      val lacksDefinition =
        rhsIsEmpty &&
          name.isTermName && !name.isConstructorName && !givenFlags.is(TermParamOrAccessor) ||
        isAbsType
      var flags = givenFlags
      if (lacksDefinition && tag != PARAM) flags |= Deferred
      if (tag == DEFDEF) flags |= Method
      if (givenFlags is Module)
        flags = flags | (if (tag == VALDEF) ModuleValCreationFlags else ModuleClassCreationFlags)
      if (ctx.owner.isClass) {
        if (tag == TYPEPARAM) flags |= Param
        else if (tag == PARAM) flags |= ParamAccessor
      }
      else if (isParamTag(tag)) flags |= Param
      flags
    }

    def isAbstractType(ttag: Int)(implicit ctx: Context): Boolean = nextUnsharedTag match {
      case LAMBDAtpt =>
        val rdr = fork
        rdr.reader.readByte()  // tag
        rdr.reader.readNat()   // length
        rdr.skipParams()       // tparams
        rdr.isAbstractType(rdr.nextUnsharedTag)
      case TYPEBOUNDS | TYPEBOUNDStpt => true
      case _ => false
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
      var name: Name = readName()
      if (tag == TYPEDEF || tag == TYPEPARAM) name = name.toTypeName
      skipParams()
      val ttag = nextUnsharedTag
      val isAbsType = isAbstractType(ttag)
      val isClass = ttag == TEMPLATE
      val templateStart = currentAddr
      skipTree() // tpt
      val rhsStart = currentAddr
      val rhsIsEmpty = noRhs(end)
      if (!rhsIsEmpty) skipTree()
      val (givenFlags, annots, privateWithin) = readModifiers(end)
      pickling.println(i"creating symbol $name at $start with flags $givenFlags")
      val flags = normalizeFlags(tag, givenFlags, name, isAbsType, rhsIsEmpty)
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

            val coord = coordAt(start)

            if (isClass)
              ctx.newClassSymbol(ctx.owner, name.asTypeName, flags, completer, privateWithin, coord)
            else
              ctx.newSymbol(ctx.owner, name, flags, completer, privateWithin, coord)
        }
      sym.annotations = annots
      ctx.owner match {
        case cls: ClassSymbol => cls.enter(sym)
        case _ =>
      }
      registerSym(start, sym)
      if (isClass) {
        sym.completer.withDecls(newScope)
        forkAt(templateStart).indexTemplateParams()(localContext(sym))
      }
      else if (sym.isInlineMethod)
        sym.addAnnotation(LazyBodyAnnotation { ctx0 =>
          implicit val ctx: Context = localContext(sym)(ctx0).addMode(Mode.ReadPositions)
            // avoids space leaks by not capturing the current context
          forkAt(rhsStart).readTerm()
        })
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
          case MACRO => addFlag(Macro)
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
            val tp = readType()
            val lazyAnnotTree = readLater(end, rdr => ctx => rdr.readTerm()(ctx))
            annots += Annotation.deferredSymAndTree(
              implicit ctx => tp.typeSymbol,
              implicit ctx => lazyAnnotTree.complete)
          case tag =>
            assert(false, s"illegal modifier tag $tag at $currentAddr, end = $end")
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

    private def readNewDef()(implicit ctx: Context): Tree = {
      val start = currentAddr
      val sym = symAtAddr(start)
      val tag = readByte()
      val end = readEnd()

      def readParamss(implicit ctx: Context): List[List[ValDef]] = {
        collectWhile(nextByte == PARAMS) {
          readByte()
          readEnd()
          readParams[ValDef](PARAM)
        }
      }

      val localCtx = localContext(sym)

      def readRhs(implicit ctx: Context) =
        if (noRhs(end)) EmptyTree
        else readLater(end, rdr => ctx => rdr.readTerm()(ctx))

      def ValDef(tpt: Tree) =
        ta.assignType(untpd.ValDef(sym.name.asTermName, tpt, readRhs(localCtx)), sym)

      def DefDef(tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree) =
         ta.assignType(
            untpd.DefDef(sym.name.asTermName, tparams, vparamss, tpt, readRhs(localCtx)),
            sym)

      def TypeDef(rhs: Tree) =
        ta.assignType(untpd.TypeDef(sym.name.asTypeName, rhs), sym)

      def ta =  ctx.typeAssigner

      val name = readName()
      pickling.println(s"reading def of $name at $start")
      val tree: MemberDef = tag match {
        case DEFDEF =>
          val tparams = readParams[TypeDef](TYPEPARAM)(localCtx)
          val vparamss = readParamss(localCtx)
          val tpt = readTpt()(localCtx)
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
          val tpt = readTpt()(localCtx)
          sym.info = tpt.tpe
          ValDef(tpt)
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
              if (sym is Flags.ModuleClass) sym.registerCompanionMethod(nme.COMPANION_CLASS_METHOD, companion)
              else sym.registerCompanionMethod(nme.COMPANION_MODULE_METHOD, companion)
            }
            TypeDef(readTemplate(localCtx))
          } else {
            val rhs = readTpt()(localCtx)
            sym.info = NoCompleter
            sym.info = rhs.tpe match {
              case _: TypeBounds | _: ClassInfo => checkNonCyclic(sym, rhs.tpe, reportErrors = false)
              case _ => TypeAlias(rhs.tpe)
            }
            TypeDef(rhs)
          }
        case PARAM =>
          val tpt = readTpt()(localCtx)
          if (noRhs(end)) {
            sym.info = tpt.tpe
            ValDef(tpt)
          }
          else {
            sym.setFlag(Method)
            sym.info = ExprType(tpt.tpe)
            pickling.println(i"reading param alias $name -> $currentAddr")
            DefDef(Nil, Nil, tpt)
          }
      }
      val mods =
        if (sym.annotations.isEmpty) untpd.EmptyModifiers
        else untpd.Modifiers(annotations = sym.annotations.map(_.tree))
      tree.withMods(mods)
        // record annotations in tree so that tree positions can be filled in.
        // Note: Once the inline PR with its changes to positions is in, this should be
        // no longer necessary.
      goto(end)
      setPos(start, tree)
      if (!sym.isType) { // Only terms might have leaky aliases, see the documentation of `checkNoPrivateLeaks`
        sym.info = ta.avoidPrivateLeaks(sym, tree.pos)
      }
      if ((sym.isClass || sym.is(CaseVal)) && sym.isLocal)
        // Child annotations for local classes and enum values are not pickled, so
        // need to be re-established here.
        sym.registerIfChild(late = true)
      tree
    }

    private def readTemplate(implicit ctx: Context): Template = {
      val start = currentAddr
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
      val parents = collectWhile(nextByte != SELFDEF && nextByte != DEFDEF) {
        nextUnsharedTag match {
          case APPLY | TYPEAPPLY => readTerm()(parentCtx)
          case _ => readTpt()(parentCtx)
        }
      }
      val parentTypes = parents.map(_.tpe.dealias)
      val self =
        if (nextByte == SELFDEF) {
          readByte()
          untpd.ValDef(readName(), readTpt(), EmptyTree).withType(NoType)
        }
        else EmptyValDef
      cls.info = ClassInfo(cls.owner.thisType, cls, parentTypes, cls.unforcedDecls,
        if (self.isEmpty) NoType else self.tpt.tpe)
      cls.setNoInitsFlags(fork.indexStats(end))
      val constr = readIndexedDef().asInstanceOf[DefDef]
      val mappedParents = parents.map(_.changeOwner(localDummy, constr.symbol))

      val lazyStats = readLater(end, rdr => implicit ctx => {
        val stats = rdr.readIndexedStats(localDummy, end)
        tparams ++ vparams ++ stats
      })
      setPos(start,
        untpd.Template(constr, mappedParents, self, lazyStats)
          .withType(localDummy.termRef))
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
      val start = currentAddr
      readByte()
      readEnd()
      val expr = readTerm()
      def readSelectors(): List[untpd.Tree] = nextByte match {
        case IMPORTED =>
          val start = currentAddr
          readByte()
          val from = setPos(start, untpd.Ident(readName()))
          nextByte match {
            case RENAMED =>
              val start2 = currentAddr
              readByte()
              val to = setPos(start2, untpd.Ident(readName()))
              untpd.Thicket(from, to) :: readSelectors()
            case _ =>
              from :: readSelectors()
          }
          case _ =>
            Nil
      }
      setPos(start, Import(expr, readSelectors()))
    }

    def readIndexedStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] =
      until(end)(readIndexedStat(exprOwner))

    def readStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] = {
      fork.indexStats(end)
      readIndexedStats(exprOwner, end)
    }

    def readIndexedParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] =
      collectWhile(nextByte == tag) { readIndexedDef().asInstanceOf[T] }

    def readParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] = {
      fork.indexParams(tag)
      readIndexedParams(tag)
    }

// ------ Reading trees -----------------------------------------------------

    def readTerm()(implicit ctx: Context): Tree = {  // TODO: rename to readTree
      val start = currentAddr
      val tag = readByte()
      pickling.println(s"reading term ${astTagToString(tag)} at $start")

      def readPathTerm(): Tree = {
        goto(start)
        readType() match {
          case path: TypeRef => TypeTree(path)
          case path: TermRef => ref(path)
          case path: ThisType => untpd.This(untpd.EmptyTypeIdent).withType(path)
          case path: ConstantType => Literal(path.value)
        }
      }

      def completeSelect(name: Name, tpf: Type => NamedType): Select = {
        val localCtx =
          if (name == nme.CONSTRUCTOR) ctx.addMode(Mode.InSuperCall) else ctx
        val qual = readTerm()(localCtx)
        untpd.Select(qual, name).withType(tpf(qual.tpe.widenIfUnstable))
      }

      def readQualId(): (untpd.Ident, TypeRef) = {
        val qual = readTerm().asInstanceOf[untpd.Ident]
         (untpd.Ident(qual.name).withPos(qual.pos), qual.tpe.asInstanceOf[TypeRef])
      }

      def accessibleDenot(pre: Type, name: Name, sig: Signature) = {
        val d = pre.member(name).atSignature(sig)
        if (!d.symbol.exists || d.symbol.isAccessibleFrom(pre)) d
        else pre.nonPrivateMember(name).atSignature(sig)
      }

      def readSimpleTerm(): Tree = tag match {
        case SHAREDterm =>
          forkAt(readAddr()).readTerm()
        case IDENT =>
          untpd.Ident(readName()).withType(readType())
        case IDENTtpt =>
          untpd.Ident(readName().toTypeName).withType(readType())
        case SELECT =>
          def readRest(name: TermName, sig: Signature): Tree =
            completeSelect(name, pre => TermRef(pre, name, accessibleDenot(pre, name, sig)))
          readName() match {
            case SignedName(name, sig) => readRest(name, sig)
            case name => readRest(name, Signature.NotAMethod)
          }
        case SELECTtpt =>
          val name = readName().toTypeName
          completeSelect(name, pre => TypeRef(pre, name, accessibleDenot(pre, name, Signature.NotAMethod)))
        case QUALTHIS =>
          val (qual, tref) = readQualId()
          untpd.This(qual).withType(ThisType.raw(tref))
        case NEW =>
          New(readTpt())
        case SINGLETONtpt =>
          SingletonTypeTree(readTerm())
        case BYNAMEtpt =>
          ByNameTypeTree(readTpt())
        case NAMEDARG =>
          NamedArg(readName(), readTerm())
        case _ =>
          readPathTerm()
      }

      def readLengthTerm(): Tree = {
        val end = readEnd()

        def readBlock(mkTree: (List[Tree], Tree) => Tree): Tree = {
          val exprReader = fork
          skipTree()
          val stats = readStats(ctx.owner, end)
          val expr = exprReader.readTerm()
          mkTree(stats, expr)
        }

        val result =
          (tag: @switch) match {
            case SUPER =>
              val qual = readTerm()
              val (mixId, mixTpe) = ifBefore(end)(readQualId(), (untpd.EmptyTypeIdent, NoType))
              tpd.Super(qual, mixId, ctx.mode.is(Mode.InSuperCall), mixTpe.typeSymbol)
            case APPLY =>
              val fn = readTerm()
              tpd.Apply(fn, until(end)(readTerm()))
            case TYPEAPPLY =>
              tpd.TypeApply(readTerm(), until(end)(readTpt()))
            case TYPED =>
              val expr = readTerm()
              val tpt = readTpt()
              Typed(expr, tpt)
            case ASSIGN =>
              Assign(readTerm(), readTerm())
            case BLOCK =>
              readBlock(Block)
            case INLINED =>
              val call = readTerm()
              readBlock((defs, expr) => Inlined(call, defs.asInstanceOf[List[MemberDef]], expr))
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
            case SELECTouter =>
              val levels = readNat()
              readTerm().outerSelect(levels, SkolemType(readType()))
            case REPEATED =>
              val elemtpt = readTpt()
              SeqLiteral(until(end)(readTerm()), elemtpt)
            case BIND =>
              val name = readName()
              val info = readType()
              val sym = ctx.newSymbol(ctx.owner, name, EmptyFlags, info, coord = coordAt(start))
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
            case REFINEDtpt =>
              val refineCls = ctx.newRefinedClassSymbol(coordAt(start))
              typeAtAddr(start) = refineCls.typeRef
              val parent = readTpt()
              val refinements = readStats(refineCls, end)(localContext(refineCls))
              RefinedTypeTree(parent, refinements, refineCls)
            case APPLIEDtpt =>
              // If we do directly a tpd.AppliedType tree we might get a
              // wrong number of arguments in some scenarios reading F-bounded
              // types. This came up in #137 of collection strawman.
              val tycon = readTpt()
              val args = until(end)(readTpt())
              untpd.AppliedTypeTree(tycon, args).withType(tycon.tpe.safeAppliedTo(args.tpes))
            case ANDtpt =>
              val tpt1 = readTpt()
              val tpt2 = readTpt()
              // FIXME: We need to do this instead of "AndType(tpt1, tpt2)" to avoid self-type cyclic reference in tasty_tools
              untpd.AndTypeTree(tpt1, tpt2).withType(AndType(tpt1.tpe, tpt2.tpe))
            case ORtpt =>
              OrTypeTree(readTpt(), readTpt())
            case ANNOTATEDtpt =>
              Annotated(readTpt(), readTerm())
            case LAMBDAtpt =>
              val tparams = readParams[TypeDef](TYPEPARAM)
              val body = readTpt()
              LambdaTypeTree(tparams, body)
            case TYPEBOUNDStpt =>
              TypeBoundsTree(readTpt(), readTpt())
            case HOLE =>
              val idx = readNat()
              val args = until(end)(readTerm())
              val splice = splices(idx)
              val expr =
                if (args.isEmpty) splice.asInstanceOf[Expr[_]]
                else splice.asInstanceOf[Seq[Any] => Expr[_]](args.map(RawQuoted.apply))
              PickledQuotes.quotedToTree(expr)
            case _ =>
              readPathTerm()
          }
        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      val tree = if (tag < firstLengthTreeTag) readSimpleTerm() else readLengthTerm()
      if (!tree.isInstanceOf[TypTree]) // FIXME: Necessary to avoid self-type cyclic reference in tasty_tools
        tree.overwriteType(tree.tpe.simplified)
      setPos(start, tree)
    }

    def readTpt()(implicit ctx: Context) =
      if (nextByte == SHAREDterm || isTypeTreeTag(nextUnsharedTag)) readTerm()
      else {
        val start = currentAddr
        val tp = readType()
        if (tp.exists) setPos(start, TypeTree(tp)) else EmptyTree
      }

    def readCases(end: Addr)(implicit ctx: Context): List[CaseDef] =
      collectWhile((nextByte == CASEDEF || nextByte == SHAREDterm) && currentAddr != end) {
        if (nextByte == SHAREDterm) {
          readByte()
          forkAt(readAddr()).readCase()(ctx.fresh.setNewScope)
        }
        else readCase()(ctx.fresh.setNewScope)
      }

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

// ------ Setting positions ------------------------------------------------

    /** Pickled position for `addr`. */
    def posAt(addr: Addr)(implicit ctx: Context): Position =
      if (ctx.mode.is(Mode.ReadPositions)) {
        posUnpicklerOpt match {
          case Some(posUnpickler) =>
            posUnpickler.posAt(addr)
          case _  =>
            NoPosition
        }
      } else NoPosition

    /** Coordinate for the symbol at `addr`. */
    def coordAt(addr: Addr)(implicit ctx: Context): Coord = {
      val pos = posAt(addr)
      if (pos.exists)
        positionCoord(pos)
      else
        indexCoord(addr.index)
    }

    /** Set position of `tree` at given `addr`. */
    def setPos[T <: untpd.Tree](addr: Addr, tree: T)(implicit ctx: Context): tree.type = {
      val pos = posAt(addr)
      if (pos.exists) tree.setPosUnchecked(pos)
      tree
    }
  }

  class LazyReader[T <: AnyRef](reader: TreeReader, op: TreeReader => Context => T) extends Trees.Lazy[T] {
    def complete(implicit ctx: Context): T = {
      pickling.println(i"starting to read at ${reader.reader.currentAddr}")
      op(reader)(ctx.withPhaseNoLater(ctx.picklerPhase))
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

  /** A marker value used to detect cyclic reference while unpickling definitions. */
  @sharable val PoisonTree: tpd.Tree = Thicket(Nil)

  /** An enumeration indicating which subtrees should be added to an OwnerTree. */
  type MemberDefMode = Int
  final val MemberDefsOnly = 0   // add only member defs; skip other statements
  final val NoMemberDefs = 1     // add only statements that are not member defs
  final val AllDefs = 2          // add everything

  class TreeWithoutOwner extends Exception
}


